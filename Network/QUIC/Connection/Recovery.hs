{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.QUIC.Connection.Recovery (
    onAckReceived
  , onPacketSent
  , onPacketReceived
  , onPacketNumberSpaceDiscarded
  , releaseByRetry
  , releaseOldest
  , checkWindowOpenSTM
  , takePingSTM
  , setInitialCongestionWindow
  , resender
  , speedup
  ) where

import Control.Concurrent.STM
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import Data.UnixTime
import GHC.Event hiding (new)

import Network.QUIC.Connection.Crypto
import Network.QUIC.Connection.Misc
import Network.QUIC.Connection.PacketNumber
import Network.QUIC.Connection.Qlog
import Network.QUIC.Connection.Queue
import Network.QUIC.Connection.State
import Network.QUIC.Connection.Types
import Network.QUIC.Imports
import Network.QUIC.Timeout
import Network.QUIC.Types

-- | Maximum reordering in packets before packet threshold loss
--   detection considers a packet lost.
kPacketThreshold :: PacketNumber
kPacketThreshold = 3

-- | Maximum reordering in time before time threshold loss detection
--   considers a packet lost.  Specified as an RTT multiplier.

kTimeThreshold :: Microseconds -> Microseconds
kTimeThreshold x = x + (x .>>. 3) -- 9/8

-- | Timer granularity.
kGranularity :: Microseconds
kGranularity = Microseconds 5000

onPacketSent :: Connection -> SentPacket -> IO ()
onPacketSent conn@Connection{..} sentPacket = do
    let lvl0 = spEncryptionLevel sentPacket
    let lvl | lvl0 == RTT0Level = RTT1Level
            | otherwise         = lvl0
    discarded <- getPacketNumberSpaceDiscarded conn lvl
    unless discarded $ do
        onPacketSentCC conn sentPacket
        when (spAckEliciting sentPacket) $
            atomicModifyIORef'' (lossDetection ! lvl) $ \ld -> ld {
                timeOfLastAckElicitingPacket = spTimeSent sentPacket
              }
        atomicModifyIORef'' (sentPackets ! lvl) $
            \(SentPackets db) -> SentPackets (db |> sentPacket)
        setLossDetectionTimer conn lvl

-- fixme
serverIsAtAntiAmplificationLimit :: Bool
serverIsAtAntiAmplificationLimit = False

onPacketReceived :: Connection -> EncryptionLevel -> PacketNumber -> IO ()
onPacketReceived conn lvl pn = do
  addPeerPacketNumbers conn lvl pn
  -- If this datagram unblocks the server, arm the
  -- PTO timer to avoid deadlock.
  when serverIsAtAntiAmplificationLimit $ setLossDetectionTimer conn lvl

onAckReceived :: Connection -> EncryptionLevel -> AckInfo -> Microseconds -> IO ()
onAckReceived conn@Connection{..} lvl ackInfo@(AckInfo largestAcked _ _) ackDelay = do
    changed <- atomicModifyIORef' (lossDetection ! lvl) update
    when changed $ do
        let predicate = fromAckInfoToPred ackInfo . spPacketNumber
        releaseLostCandidates conn lvl predicate >>= updateCC
        releaseByPredicate    conn lvl predicate >>= detectLossUpdateCC
  where
    update ld@LossDetection{..} = (ld', changed)
      where
        ld' = ld { largestAckedPacket = max largestAckedPacket largestAcked
                 , previousAckInfo = ackInfo
                 }
        changed = previousAckInfo /= ackInfo
    detectLossUpdateCC newlyAckedPackets = case Seq.viewr newlyAckedPackets of
      EmptyR -> return ()
      _ :> lastPkt -> do
          -- If the largest acknowledged is newly acked and
          -- at least one ack-eliciting was newly acked, update the RTT.
          when (spPacketNumber lastPkt == largestAcked
             && any spAckEliciting newlyAckedPackets) $ do
              rtt <- getElapsedTimeMicrosecond $ spTimeSent lastPkt
              let latestRtt = max rtt kGranularity
              updateRTT conn lvl latestRtt ackDelay

          {- fimxe
          -- Process ECN information if present.
          if (ACK frame contains ECN information):
             ProcessECN(ack, lvl)
          -}

          lostPackets <- detectAndRemoveLostPackets conn lvl
          unless (null lostPackets) $ do
              mode <- ccMode <$> readTVarIO recoveryCC
              if lvl == RTT1Level && mode /= SlowStart then
                  mergeLostCandidates conn lostPackets
                else do
                  -- just in case
                  lostPackets' <- mergeLostCandidatesAndClear conn lostPackets
                  onPacketsLost conn lostPackets'
                  retransmit conn lostPackets'
          -- setLossDetectionTimer in updateCC
          updateCC newlyAckedPackets

    updateCC newlyAckedPackets
      | newlyAckedPackets == Seq.empty = return ()
      | otherwise = do
          onPacketsAcked conn newlyAckedPackets

          -- Sec 6.2.1. Computing PTO
          -- "The PTO backoff factor is reset when an acknowledgement is
          --  received, except in the following case. A server might
          --  take longer to respond to packets during the handshake
          --  than otherwise. To protect such a server from repeated
          --  client probes, the PTO backoff is not reset at a client
          --  that is not yet certain that the server has finished
          --  validating the client's address."
          completed <- peerCompletedAddressValidation conn
          when completed $ metricsUpdated conn $
              atomicModifyIORef'' recoveryRTT $ \rtt -> rtt { ptoCount = 0 }

          setLossDetectionTimer conn lvl

updateRTT :: Connection -> EncryptionLevel -> Microseconds -> Microseconds -> IO ()
updateRTT conn@Connection{..} lvl latestRTT0 ackDelay0 = metricsUpdated conn $ do
    firstTime <- atomicModifyIORef' recoveryRTT update
    when firstTime $ do
        setPktNumPersistent conn
        qlogDebug conn $ Debug "RTT first sample"
  where
    -- don't use latestRTT, use latestRTT0 instead
    --
    -- First time:
    -- Overwriting the initial value with the first sample.
    -- Initial value was used to calculate PTO.
    --
    -- smoothed_rtt = rtt_sample
    -- rttvar = rtt_sample / 2
    update rtt@RTT{..} | latestRTT == Microseconds 0 = (rtt {
        latestRTT   = latestRTT0
      , minRTT      = latestRTT0
      , smoothedRTT = latestRTT0
      , rttvar      = latestRTT0 `unsafeShiftR` 1
      }, True)
    -- Others:
    update rtt@RTT{..} = (rtt {
        latestRTT   = latestRTT0
      , minRTT      = minRTT'
      , smoothedRTT = smoothedRTT'
      , rttvar      = rttvar'
      }, False)
      where
        -- minRTT ignores ack delay.
        minRTT' = min minRTT latestRTT0
        -- Limit ack_delay by max_ack_delay
        -- ack_delay = min(Ack Delay in ACK Frame, max_ack_delay)
        ackDelay = min ackDelay0 $ getMaxAckDelay (Just lvl) maxAckDelay1RTT
        -- Adjust for ack delay if plausible.
        -- adjusted_rtt = latest_rtt
        -- if (min_rtt + ack_delay < latest_rtt):
        --   adjusted_rtt = latest_rtt - ack_delay
        adjustedRTT
          | latestRTT0 > minRTT + ackDelay = latestRTT0 - ackDelay
          | otherwise                      = latestRTT0
        -- rttvar_sample = abs(smoothed_rtt - adjusted_rtt)
        -- rttvar = 3/4 * rttvar + 1/4 * rttvar_sample
        rttvar' = rttvar - (rttvar .>>. 2)
                + (abs (smoothedRTT - adjustedRTT) .>>. 2)
        -- smoothed_rtt = 7/8 * smoothed_rtt + 1/8 * adjusted_rtt
        smoothedRTT' = smoothedRTT - (smoothedRTT .>>. 3)
                     + (adjustedRTT .>>. 3)

detectAndRemoveLostPackets :: Connection -> EncryptionLevel -> IO (Seq SentPacket)
detectAndRemoveLostPackets conn@Connection{..} lvl = do
    lae <- timeOfLastAckElicitingPacket <$> readIORef (lossDetection ! lvl)
    when (lae == timeMicrosecond0) $
        qlogDebug conn $ Debug "detectAndRemoveLostPackets: timeOfLastAckElicitingPacket: 0"
    atomicModifyIORef'' (lossDetection ! lvl) $ \ld -> ld {
          lossTime = Nothing
        }
    RTT{..} <- readIORef recoveryRTT
    LossDetection{..} <- readIORef (lossDetection ! lvl)
    when (largestAckedPacket == -1) $
        qlogDebug conn $ Debug "detectAndRemoveLostPackets: largestAckedPacket: -1"
    -- Sec 6.1.2. Time Threshold
    -- max(kTimeThreshold * max(smoothed_rtt, latest_rtt), kGranularity)
    let lossDelay0 = kTimeThreshold $ max latestRTT smoothedRTT
    let lossDelay = max lossDelay0 kGranularity

    tm <- getPastTimeMicrosecond lossDelay
    let predicate ent = (spPacketNumber ent <= largestAckedPacket - kPacketThreshold)
                     || (spPacketNumber ent <= largestAckedPacket && spTimeSent ent <= tm)
    lostPackets <- releaseByPredicate conn lvl predicate

    mx <- findOldest conn lvl (\x -> spPacketNumber x <= largestAckedPacket)
    case mx of
      -- No gap packet. PTO turn.
      Nothing -> return ()
      -- There are gap packets which are not declared lost.
      -- Set lossTime to next.
      Just x  -> do
          let next = spTimeSent x `addMicroseconds` lossDelay
          atomicModifyIORef'' (lossDetection ! lvl) $ \ld -> ld {
                lossTime = Just next
              }

    unless (Seq.null lostPackets) $ qlogDebug conn $ Debug "loss detected"
    return lostPackets

getLossTimeAndSpace :: Connection -> IO (Maybe (TimeMicrosecond,EncryptionLevel))
getLossTimeAndSpace Connection{..} =
    loop [InitialLevel, HandshakeLevel, RTT1Level] Nothing
  where
    loop []     r = return r
    loop (l:ls) r = do
        mt <- lossTime <$> readIORef (lossDetection ! l)
        case mt of
          Nothing -> loop ls r
          Just t  -> case r of
            Nothing -> loop ls $ Just (t,l)
            Just (t0,_)
               | t < t0    -> loop ls $ Just (t,l)
               | otherwise -> loop ls r

getMaxAckDelay :: Maybe EncryptionLevel -> Microseconds -> Microseconds
getMaxAckDelay Nothing n = n
getMaxAckDelay (Just lvl) n
  | lvl `elem` [InitialLevel,HandshakeLevel] = 0
  | otherwise                                = n

-- Sec 6.2.1. Computing PTO
-- PTO = smoothed_rtt + max(4*rttvar, kGranularity) + max_ack_delay
calcPTO :: RTT -> Maybe EncryptionLevel -> Microseconds
calcPTO RTT{..} mlvl = smoothedRTT + max (rttvar .<<. 2) kGranularity + dly
  where
    dly = getMaxAckDelay mlvl maxAckDelay1RTT

backOff :: Microseconds -> Int -> Microseconds
backOff n cnt = n * (2 ^ cnt)

getPtoTimeAndSpace :: Connection -> IO (Maybe (TimeMicrosecond, EncryptionLevel))
getPtoTimeAndSpace conn@Connection{..} = do
    -- Arm PTO from now when there are no inflight packets.
    CC{..} <- readTVarIO recoveryCC
    if bytesInFlight <= 0 then do
        validated <- peerCompletedAddressValidation conn
        if validated then do
            connDebugLog "getPtoTimeAndSpace: validated"
            return Nothing
          else do
            rtt <- readIORef recoveryRTT
            lvl <- getEncryptionLevel conn
            let pto = backOff (calcPTO rtt $ Just lvl) (ptoCount rtt)
            ptoTime <- getFutureTimeMicrosecond pto
            return $ Just (ptoTime, lvl)
      else do
        completed <- isConnectionEstablished conn
        let lvls | completed = [InitialLevel, HandshakeLevel, RTT1Level]
                 | otherwise = [InitialLevel, HandshakeLevel]
        loop lvls
  where
    loop :: [EncryptionLevel] -> IO (Maybe (TimeMicrosecond, EncryptionLevel))
    loop [] = return Nothing
    loop (l:ls) = do
        notInFlight <- noInFlightPacket conn l
        if notInFlight then
            loop ls
          else do
            LossDetection{..} <- readIORef (lossDetection ! l)
            if timeOfLastAckElicitingPacket == timeMicrosecond0 then
                loop ls
              else do
                  rtt <- readIORef recoveryRTT
                  let pto = backOff (calcPTO rtt $ Just l) (ptoCount rtt)
                  let ptoTime = timeOfLastAckElicitingPacket `addMicroseconds` pto
                  return $ Just (ptoTime, l)

-- Sec 6.2.1. Computing PTO
-- "That is, a client does not reset the PTO backoff factor on
--  receiving acknowledgements until it receives a HANDSHAKE_DONE
--  frame or an acknowledgement for one of its Handshake or 1-RTT
--  packets."
peerCompletedAddressValidation :: Connection -> IO Bool
-- For servers: assume clients validate the server's address implicitly.
peerCompletedAddressValidation conn
  | isServer conn = return True
-- For clients: servers complete address validation when a protected
-- packet is received.
-- has received Handshake ACK (fixme)
-- has received 1-RTT ACK     (fixme)
-- has received HANDSHAKE_DONE
peerCompletedAddressValidation conn = isConnectionEstablished conn

cancelLossDetectionTimer :: Connection -> IO ()
cancelLossDetectionTimer conn@Connection{..} = do
    mk <- atomicModifyIORef' timerKey $ \oldkey -> (Nothing, oldkey)
    case mk of
      Nothing -> return ()
      Just k -> do
          mgr <- getSystemTimerManager
          unregisterTimeout mgr k
          oldtmi <- readIORef timerInfo
          let tmi = oldtmi { timerEvent = TimerCancelled }
          writeIORef timerInfo tmi
          qlogLossTimerUpdated conn tmi

updateLossDetectionTimer :: Connection -> TimerInfo -> IO ()
updateLossDetectionTimer conn@Connection{..} tmi = do
    oldtmi <- readIORef timerInfo
    when (timerTime oldtmi /= timerTime tmi) $ do
        mgr <- getSystemTimerManager
        let Left tim = timerTime tmi
        duration@(Microseconds us) <- getTimeoutInMicrosecond tim
        if us <= 0 then do
            qlogDebug conn $ Debug "updateLossDetectionTimer: minus"
            -- cancelLossDetectionTimer conn -- don't cancel
          else do
            key <- registerTimeout mgr us (onLossDetectionTimeout conn)
            mk <- atomicModifyIORef' timerKey $ \oldkey -> (Just key, oldkey)
            case mk of
              Nothing -> return ()
              Just k -> unregisterTimeout mgr k
            let newtmi = tmi { timerTime = Right duration }
            writeIORef timerInfo newtmi
            qlogLossTimerUpdated conn newtmi

setLossDetectionTimer :: Connection -> EncryptionLevel -> IO ()
setLossDetectionTimer conn@Connection{..} lvl0 = do
    mtl <- getLossTimeAndSpace conn
    case mtl of
      Just (earliestLossTime,lvl) -> do
          when (lvl0 == lvl) $ do
              -- Time threshold loss detection.
              let tmi = TimerInfo (Left earliestLossTime) lvl LossTime TimerSet
              updateLossDetectionTimer conn tmi
      Nothing ->
          if serverIsAtAntiAmplificationLimit then -- server is at anti-amplification limit
            -- The server's timer is not set if nothing can be sent.
              cancelLossDetectionTimer conn
            else do
              CC{..} <- readTVarIO recoveryCC
              validated <- peerCompletedAddressValidation conn
              if numOfAckEliciting <= 0 && validated then
                  -- There is nothing to detect lost, so no timer is
                  -- set. However, we only do this if the peer has
                  -- been validated, to prevent the server from being
                  -- blocked by the anti-amplification limit.
                  cancelLossDetectionTimer conn
                else do
                  -- Determine which PN space to arm PTO for.
                  mx <- getPtoTimeAndSpace conn
                  case mx of
                    Nothing -> cancelLossDetectionTimer conn
                    Just (ptoTime, lvl) -> do
                        when (lvl0 == lvl) $ do
                            let tmi = TimerInfo (Left ptoTime) lvl PTO TimerSet
                            updateLossDetectionTimer conn tmi

-- The only time the PTO is armed when there are no bytes in flight is
-- when it's a client and it's unsure if the server has completed
-- address validation.
onLossDetectionTimeout :: Connection -> IO ()
onLossDetectionTimeout conn@Connection{..} = do
    open <- isConnectionOpen conn
    when open $ do
        tmi <- readIORef timerInfo
        let lvl = timerLevel tmi
        discarded <- getPacketNumberSpaceDiscarded conn lvl
        if discarded then do
            qlogLossTimerUpdated conn tmi { timerEvent = TimerCancelled }
            cancelLossDetectionTimer conn
          else
            lossTimeOrPTO lvl tmi
  where
    lossTimeOrPTO lvl tmi = do
        qlogLossTimerUpdated conn tmi { timerEvent = TimerExpired }
        case timerType tmi of
          LossTime -> do
              -- Time threshold loss Detection
              lostPackets <- detectAndRemoveLostPackets conn lvl
              when (null lostPackets) $ connDebugLog "onLossDetectionTimeout: null"
              lostPackets' <- mergeLostCandidatesAndClear conn lostPackets
              onPacketsLost conn lostPackets'
              retransmit conn lostPackets'
              setLossDetectionTimer conn lvl
          PTO -> do
              CC{..} <- readTVarIO recoveryCC
              if bytesInFlight > 0 then do
                  -- PTO. Send new data if available, else retransmit old data.
                  -- If neither is available, send a single PING frame.
                  sendPing conn lvl
                else do
                  -- Client sends an anti-deadlock packet: Initial is padded
                  -- to earn more anti-amplification credit,
                  -- a Handshake packet proves address ownership.
                  validated <- peerCompletedAddressValidation conn
                  when (validated) $ connDebugLog "onLossDetectionTimeout: RTT1"
                  lvl' <- getEncryptionLevel conn -- fixme
                  sendPing conn lvl'

              metricsUpdated conn $
                  atomicModifyIORef'' recoveryRTT $
                      \rtt -> rtt { ptoCount = ptoCount rtt + 1 }
              setLossDetectionTimer conn lvl

sendPing :: Connection -> EncryptionLevel -> IO ()
sendPing Connection{..} lvl = do
    now <- getTimeMicrosecond
    atomicModifyIORef'' (lossDetection ! lvl) $ \ld -> ld {
        timeOfLastAckElicitingPacket = now
      }
    atomically $ writeTVar ptoPing $ Just lvl

----------------------------------------------------------------
----------------------------------------------------------------

-- | Default limit on the initial bytes in flight.
kInitialWindow :: Int -> Int
--kInitialWindow pktSiz = min 14720 (10 * pktSiz)
kInitialWindow pktSiz = pktSiz .<<. 2 --  .<<. 1 is not good enough

-- | Minimum congestion window in bytes.
kMinimumWindow :: Connection -> IO Int
kMinimumWindow Connection{..} = do
    siz <- readIORef maxPacketSize
    return (siz .<<. 2) -- .<<. 1 is not good enough

-- | Reduction in congestion window when a new loss event is detected.
kLossReductionFactor :: Int -> Int
kLossReductionFactor = (.>>. 1) -- 0.5

-- | Period of time for persistent congestion to be established,
-- specified as a PTO multiplier.
kPersistentCongestionThreshold :: Microseconds -> Microseconds
kPersistentCongestionThreshold (Microseconds us) = Microseconds (3 * us)

onPacketSentCC :: Connection -> SentPacket -> IO ()
onPacketSentCC conn@Connection{..} sentPacket = metricsUpdated conn $
    atomically $ modifyTVar' recoveryCC $ \cc -> cc {
        bytesInFlight = bytesInFlight cc + bytesSent
      , numOfAckEliciting = numOfAckEliciting cc + countAckEli sentPacket
      }
  where
    bytesSent = spSentBytes sentPacket

countAckEli :: SentPacket -> Int
countAckEli sentPacket
  | spAckEliciting sentPacket = 1
  | otherwise                 = 0

inCongestionRecovery :: TimeMicrosecond -> Maybe TimeMicrosecond -> Bool
inCongestionRecovery _ Nothing = False
inCongestionRecovery sentTime (Just crst) = sentTime <= crst

onPacketsAcked :: Connection -> Seq SentPacket -> IO ()
onPacketsAcked conn@Connection{..} ackedPackets = metricsUpdated conn $ do
    maxPktSiz <- readIORef maxPacketSize
    oldcc <- readTVarIO recoveryCC
    atomically $ modifyTVar' recoveryCC $ modify maxPktSiz
    newcc <- readTVarIO recoveryCC
    when (ccMode oldcc /= ccMode newcc) $
      qlogContestionStateUpdated conn $ ccMode newcc
  where
    modify maxPktSiz cc@CC{..} = cc {
           bytesInFlight = bytesInFlight'
         , congestionWindow = congestionWindow'
         , bytesAcked = bytesAcked'
         , ccMode = ccMode'
         , numOfAckEliciting = numOfAckEliciting'
         }
      where
        (bytesInFlight',congestionWindow',bytesAcked',ccMode',numOfAckEliciting') =
              foldl' (.+) (bytesInFlight,congestionWindow,bytesAcked,ccMode,numOfAckEliciting) ackedPackets
        (bytes,cwin,acked,_,cnt) .+ sp@SentPacket{..} = (bytes',cwin',acked',mode',cnt')
          where
            isRecovery = inCongestionRecovery spTimeSent congestionRecoveryStartTime
            bytes' = bytes - spSentBytes
            ackedA = acked + spSentBytes
            cnt' = cnt - countAckEli sp
            (cwin',acked',mode')
              -- Do not increase congestion window in recovery period.
              | isRecovery      = (cwin, acked, Recovery)
              -- fixme: Do not increase congestion_window if application
              -- limited or flow control limited.
              --
              -- Slow start.
              | cwin < ssthresh = (cwin + spSentBytes, acked, SlowStart)
              -- Congestion avoidance.
              -- In this implementation, maxPktSiz == spSentBytes.
              -- spSentBytes is large enough, so we don't care
              -- the roundup issue of `div`.
              | ackedA >= cwin  = (cwin + maxPktSiz, ackedA - cwin, Avoidance)
              | otherwise       = (cwin, ackedA, Avoidance)

onCongestionEvent :: Connection -> Seq SentPacket -> Bool -> IO ()
onCongestionEvent conn@Connection{..} lostPackets isRecovery = do
    persistent <- inPersistentCongestion conn lostPackets
    when (persistent || not isRecovery) $ do
        minWindow <- kMinimumWindow conn
        now <- getTimeMicrosecond
        metricsUpdated conn $ atomically $ modifyTVar' recoveryCC $ \cc@CC{..} ->
            let halfWindow = max minWindow $ kLossReductionFactor congestionWindow
                cwin
                  | persistent = minWindow
                  | otherwise  = halfWindow
                sst            = halfWindow
                mode
                  | cwin < sst = SlowStart -- persistent
                  | otherwise  = Recovery
            in cc {
                congestionRecoveryStartTime = Just now
              , congestionWindow = cwin
              , ssthresh         = sst
              , ccMode           = mode
              , bytesAcked       = 0
              }
        CC{ccMode} <- atomically $ readTVar recoveryCC
        qlogContestionStateUpdated conn ccMode

-- Sec 7.8. Persistent Congestion
-- fixme: after the first sample
inPersistentCongestion :: Connection -> Seq SentPacket -> IO Bool
inPersistentCongestion conn@Connection{..} lostPackets = do
    pn <- getPktNumPersistent conn
    let mduration = findDuration lostPackets pn
    case mduration of
      Nothing -> return False
      Just duration -> do
          rtt <- readIORef recoveryRTT
          let pto = calcPTO rtt Nothing
              Microseconds congestionPeriod = kPersistentCongestionThreshold pto
              threshold = microSecondsToUnixDiffTime congestionPeriod
          return (duration > threshold)

diff0 :: UnixDiffTime
diff0 = UnixDiffTime 0 0

findDuration :: Seq SentPacket -> PacketNumber -> Maybe UnixDiffTime
findDuration pkts0 pn = leftEdge pkts0 diff0
  where
    leftEdge pkts diff = case Seq.viewl pkts' of
        EmptyL      -> Nothing
        l :< pkts'' -> case rightEdge (spPacketNumber l) pkts'' Nothing of
          (Nothing, pkts''') -> leftEdge pkts''' diff
          (Just r,  pkts''') -> let diff' = spTimeSent r `diffUnixTime` spTimeSent l
                                in leftEdge pkts''' diff'
      where
        (_, pkts') = Seq.breakl (\x -> spAckEliciting x && spPacketNumber x >= pn) pkts
    rightEdge n pkts Nothing = case Seq.viewl pkts of
        EmptyL -> (Nothing, Seq.empty)
        r :< pkts'
          | spPacketNumber r == n + 1 ->
              if spAckEliciting r then
                  rightEdge (n + 1) pkts' (Just r)
                else
                  rightEdge (n + 1) pkts' Nothing
          | otherwise -> (Nothing, pkts')
    rightEdge n pkts mr0 = case Seq.viewl pkts of
        EmptyL -> (mr0, Seq.empty)
        r :< pkts'
          | spPacketNumber r == n + 1 ->
              if spAckEliciting r then
                  rightEdge (n + 1) pkts' (Just r)
                else
                  rightEdge (n + 1) pkts' mr0
          | otherwise -> (mr0, pkts')

decreaseCC :: (Functor m, Foldable m) => Connection -> m SentPacket -> IO ()
decreaseCC conn@Connection{..} packets = do
    let sentBytes = sum (spSentBytes <$> packets)
        num = sum (countAckEli <$> packets)
    metricsUpdated conn $
        atomically $ modifyTVar' recoveryCC $ \cc ->
          cc {
            bytesInFlight = bytesInFlight cc - sentBytes
          , numOfAckEliciting = numOfAckEliciting cc - num
          }

onPacketsLost :: Connection -> Seq SentPacket -> IO ()
onPacketsLost conn@Connection{..} lostPackets = case Seq.viewr lostPackets of
  EmptyR -> return ()
  _ :> lastPkt -> do
    decreaseCC conn lostPackets
    isRecovery <- inCongestionRecovery (spTimeSent lastPkt) . congestionRecoveryStartTime <$> readTVarIO recoveryCC
    onCongestionEvent conn lostPackets isRecovery
    mapM_ (qlogPacketLost conn . LostPacket) lostPackets

retransmit :: Connection -> Seq SentPacket -> IO ()
retransmit conn lostPackets
  | null packetsToBeResent = getEncryptionLevel conn >>= sendPing conn
  | otherwise              = mapM_ put packetsToBeResent
  where
    packetsToBeResent = Seq.filter spAckEliciting lostPackets
    put spkt = putOutput conn $ OutRetrans $ spPlainPacket spkt

onPacketNumberSpaceDiscarded :: Connection -> EncryptionLevel -> IO ()
onPacketNumberSpaceDiscarded conn@Connection{..} lvl = do
    let (lvl',label) = case lvl of
          InitialLevel -> (HandshakeLevel,"initial")
          _            -> (RTT1Level, "handshake")
    qlogDebug conn $ Debug (label ++ " discarded")
    discardPacketNumberSpace conn lvl
    -- Remove any unacknowledged packets from flight.
    clearedPackets <- releaseByClear conn lvl
    decreaseCC conn clearedPackets
    -- Reset the loss detection and PTO timer
    writeIORef (lossDetection ! lvl) initialLossDetection
    metricsUpdated conn $
        atomicModifyIORef'' recoveryRTT $ \rtt -> rtt { ptoCount = 0 }
    setLossDetectionTimer conn lvl'

----------------------------------------------------------------
----------------------------------------------------------------

resender :: Connection -> IO ()
resender conn@Connection{..} = forever $ do
    atomically $ do
        lostPackets <- readTVar lostCandidates
        check (lostPackets /= emptySentPackets)
    delay $ Microseconds 10000 -- fixme
    packets <- atomically $ do
        SentPackets pkts <- readTVar lostCandidates
        writeTVar lostCandidates emptySentPackets
        return pkts
    when (packets /= Seq.empty) $ do
        onPacketsLost conn packets
        retransmit conn packets

mergeLostCandidates :: Connection -> Seq SentPacket -> IO ()
mergeLostCandidates Connection{..} lostPackets = atomically $ do
    SentPackets old <- readTVar lostCandidates
    let new = merge old lostPackets
    writeTVar lostCandidates $ SentPackets new

mergeLostCandidatesAndClear :: Connection -> Seq SentPacket -> IO (Seq SentPacket)
mergeLostCandidatesAndClear Connection{..} lostPackets = atomically $ do
    SentPackets old <- readTVar lostCandidates
    writeTVar lostCandidates emptySentPackets
    return $ merge old lostPackets

merge :: Seq SentPacket -> Seq SentPacket -> Seq SentPacket
merge s1 s2 = case Seq.viewl s1 of
  EmptyL   -> s2
  x :< s1' -> case Seq.viewl s2 of
    EmptyL  -> s1
    y :< s2'
      | spPacketNumber x < spPacketNumber y -> x <| merge s1' s2
      | otherwise                           -> y <| merge s1 s2'

releaseLostCandidates :: Connection -> EncryptionLevel -> (SentPacket -> Bool) -> IO (Seq SentPacket)
releaseLostCandidates conn@Connection{..} lvl predicate = do
    packets <- atomically $ do
        SentPackets db <- readTVar lostCandidates
        let (pkts, db') = Seq.partition predicate db
        writeTVar lostCandidates $ SentPackets db'
        return pkts
    removePacketNumbers conn lvl packets
    return packets

----------------------------------------------------------------
----------------------------------------------------------------

releaseByPredicate :: Connection -> EncryptionLevel -> (SentPacket -> Bool) -> IO (Seq SentPacket)
releaseByPredicate conn@Connection{..} lvl predicate = do
    packets <- atomicModifyIORef' (sentPackets ! lvl) $ \(SentPackets db) ->
       let (pkts, db') = Seq.partition predicate db
       in (SentPackets db', pkts)
    removePacketNumbers conn lvl packets
    return $ packets

removePacketNumbers :: Foldable t => Connection -> EncryptionLevel -> t SentPacket -> IO ()
removePacketNumbers conn lvl packets = mapM_ reduce packets
  where
    reduce x = reducePeerPacketNumbers conn lvl ppns
      where
        ppns = spPeerPacketNumbers x

----------------------------------------------------------------

releaseByClear :: Connection -> EncryptionLevel -> IO (Seq SentPacket)
releaseByClear conn@Connection{..} lvl = do
    clearPeerPacketNumbers conn lvl
    atomicModifyIORef' (sentPackets ! lvl) $ \(SentPackets db) ->
        (emptySentPackets, db)

----------------------------------------------------------------

releaseByRetry :: Connection -> IO (Seq PlainPacket)
releaseByRetry conn@Connection{..} = do
    packets <- releaseByClear conn InitialLevel
    decreaseCC conn packets
    writeIORef (lossDetection ! InitialLevel) initialLossDetection
    metricsUpdated conn $
        atomicModifyIORef'' recoveryRTT $ \rtt -> rtt { ptoCount = 0 }
    return (spPlainPacket <$> packets)

----------------------------------------------------------------

speedup :: Connection -> EncryptionLevel -> String -> IO ()
speedup conn@Connection{..} lvl desc = do
    setSpeedingUp conn
    qlogDebug conn $ Debug desc
    packets <- atomicModifyIORef' (sentPackets ! lvl) $
                  \(SentPackets db) -> (emptySentPackets, db)
    -- don't clear PeerPacketNumbers.
    unless (null packets) $ do
        onPacketsLost conn packets
        retransmit conn packets
        setLossDetectionTimer conn lvl

----------------------------------------------------------------

-- Returning the oldest if it is ack-eliciting.
releaseOldest :: Connection -> EncryptionLevel -> IO (Maybe SentPacket)
releaseOldest conn@Connection{..} lvl = do
    mr <- atomicModifyIORef' (sentPackets ! lvl) oldest
    case mr of
      Nothing   -> return ()
      Just spkt -> do
          delPeerPacketNumbers conn lvl $ spPacketNumber spkt
          decreaseCC conn [spkt]
    return mr
  where
    oldest (SentPackets db) = case Seq.viewl db2 of
      x :< db2' -> let db' = db1 >< db2'
                   in (SentPackets db', Just x)
      _         ->    (SentPackets db, Nothing)
      where
        (db1, db2) = Seq.breakl spAckEliciting db

findOldest :: Connection -> EncryptionLevel -> (SentPacket -> Bool)
           -> IO (Maybe SentPacket)
findOldest Connection{..} lvl p = oldest <$> readIORef (sentPackets ! lvl)
  where
    oldest (SentPackets db) = case Seq.viewl $ Seq.filter p db of
      EmptyL -> Nothing
      x :< _ -> Just x

----------------------------------------------------------------

noInFlightPacket :: Connection -> EncryptionLevel -> IO Bool
noInFlightPacket Connection{..} lvl = do
    SentPackets db <- readIORef (sentPackets ! lvl)
    return $ Seq.null db

----------------------------------------------------------------

takePingSTM :: Connection -> STM EncryptionLevel
takePingSTM Connection{..} = do
    mx <- readTVar ptoPing
    check $ isJust mx
    writeTVar ptoPing Nothing
    return $ fromJust mx

checkWindowOpenSTM :: Connection -> Int -> STM ()
checkWindowOpenSTM Connection{..} siz = do
    CC{..} <- readTVar recoveryCC
    check (siz <= congestionWindow - bytesInFlight)

setInitialCongestionWindow :: Connection -> Int -> IO ()
setInitialCongestionWindow conn@Connection{..} pktSiz = metricsUpdated conn $
    atomically $ do modifyTVar' recoveryCC $ \cc -> cc {
        congestionWindow = kInitialWindow pktSiz
      }

----------------------------------------------------------------

metricsUpdated :: Connection -> IO () -> IO ()
metricsUpdated conn@Connection{..} body = do
    rtt0 <- readIORef recoveryRTT
    cc0 <- readTVarIO recoveryCC
    body
    rtt1 <- readIORef recoveryRTT
    cc1 <- readTVarIO recoveryCC
    let diff = catMaybes [
            time "min_rtt"      (minRTT      rtt0) (minRTT      rtt1)
          , time "smoothed_rtt" (smoothedRTT rtt0) (smoothedRTT rtt1)
          , time "latest_rtt"   (latestRTT   rtt0) (latestRTT   rtt1)
          , time "rtt_variance" (rttvar      rtt0) (rttvar      rtt1)
          , numb "pto_count"    (ptoCount    rtt0) (ptoCount    rtt1)
          , numb "bytes_in_flight"   (bytesInFlight cc0) (bytesInFlight cc1)
          , numb "congestion_window" (congestionWindow cc0) (congestionWindow cc1)
          , numb "ssthresh"          (ssthresh cc0) (ssthresh cc1)
          ]
    unless (null diff) $ qlogMetricsUpdated conn $ MetricsDiff diff
  where
    time tag (Microseconds v0) (Microseconds v1)
      | v0 == v1  = Nothing
      | otherwise = Just (tag,v1)
    numb tag v0 v1
      | v0 == v1  = Nothing
      | otherwise = Just (tag,v1)
