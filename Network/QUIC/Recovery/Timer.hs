{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Network.QUIC.Recovery.Timer (
    getLossTimeAndSpace
  , getPtoTimeAndSpace
  , setLossDetectionTimer
  , beforeAntiAmp
  , ldccTimer
  ) where

import Control.Concurrent.STM
import qualified Data.Sequence as Seq
import Network.QUIC.Event

import Network.QUIC.Connector
import Network.QUIC.Imports
import Network.QUIC.Qlog
import Network.QUIC.Recovery.Detect
import Network.QUIC.Recovery.Metrics
import Network.QUIC.Recovery.Misc
import Network.QUIC.Recovery.Persistent
import Network.QUIC.Recovery.Release
import Network.QUIC.Recovery.Types
import Network.QUIC.Recovery.Utils
import Network.QUIC.Recovery.Constants
import Network.QUIC.Types

import           GHC.Stack

----------------------------------------------------------------

noInFlightPacket :: LDCC -> EncryptionLevel -> IO Bool
noInFlightPacket LDCC{..} lvl = do
    SentPackets db <- readIORef (sentPackets ! lvl)
    return $ Seq.null db

getLossTimeAndSpace :: LDCC -> IO (Maybe (TimeMicrosecond,EncryptionLevel))
getLossTimeAndSpace LDCC{..} =
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

----------------------------------------------------------------

getPtoTimeAndSpace :: LDCC -> IO (Maybe (TimeMicrosecond, EncryptionLevel))
getPtoTimeAndSpace ldcc@LDCC{..} = do
    -- Arm PTO from now when there are no inflight packets.
    CC{..} <- readTVarIO recoveryCC
    if bytesInFlight <= 0 then do
        validated <- peerCompletedAddressValidation ldcc
        if validated then do
            qlogDebug ldcc $ Debug "getPtoTimeAndSpace: validated"
            return Nothing
          else do
            rtt <- readIORef recoveryRTT
            lvl <- getEncryptionLevel ldcc
            let pto = backOff (calcPTO rtt $ Just lvl) (ptoCount rtt)
            ptoTime <- getFutureTimeMicrosecond pto
            return $ Just (ptoTime, lvl)
      else do
        completed <- isConnectionEstablished ldcc
        let lvls | completed = [InitialLevel, HandshakeLevel, RTT1Level]
                 | otherwise = [InitialLevel, HandshakeLevel]
        loop lvls
  where
    loop :: [EncryptionLevel] -> IO (Maybe (TimeMicrosecond, EncryptionLevel))
    loop [] = return Nothing
    loop (l:ls) = do
        notInFlight <- noInFlightPacket ldcc l
        if notInFlight then
            loop ls
          else do
            LossDetection{..} <- readIORef (lossDetection ! l)
            if timeOfLastAckElicitingPacket == timeMicrosecond0 then
                loop ls
              else do
                  rtt <- readIORef recoveryRTT
                  let pto0 = backOff (calcPTO rtt $ Just l) (ptoCount rtt)
                      pto = max pto0 kGranularity
                      ptoTime = timeOfLastAckElicitingPacket `addMicroseconds` pto
                  return $ Just (ptoTime, l)

----------------------------------------------------------------

cancelLossDetectionTimer :: LDCC -> IO ()
cancelLossDetectionTimer ldcc@LDCC{..} = do
    atomically $ writeTVar timerInfoQ Empty
    mk <- atomicModifyIORef' timerKey (Nothing,)
    forM_ mk $ \k -> do
        mgr <- getSystemTimerManager
        unregisterTimeout mgr k
        writeIORef timerInfo Nothing
        qlogLossTimerCancelled ldcc

updateLossDetectionTimer :: LDCC -> TimerInfo -> IO ()
updateLossDetectionTimer ldcc@LDCC{..} tmi = do
    mtmi <- readIORef timerInfo
    when (mtmi /= Just tmi) $ do
        if timerLevel tmi == RTT1Level then
            atomically $ writeTVar timerInfoQ $ Next tmi
          else
            updateLossDetectionTimer' ldcc tmi

ldccTimer :: LDCC -> IO ()
ldccTimer ldcc@LDCC{..} = forever $ do
    atomically $ do
        x <- readTVar timerInfoQ
        check (x /= Empty)
    delay timerGranularity
    updateWithNext ldcc

updateWithNext :: LDCC -> IO ()
updateWithNext ldcc@LDCC{..} = do
    x <- readTVarIO timerInfoQ
    case x of
      Empty    -> return ()
      Next tmi -> updateLossDetectionTimer' ldcc tmi

updateLossDetectionTimer' :: HasCallStack => LDCC -> TimerInfo -> IO ()
updateLossDetectionTimer' ldcc@LDCC{..} tmi = do
    atomically $ writeTVar timerInfoQ Empty
    let tim = timerTime tmi
    Microseconds us0 <- getTimeoutInMicrosecond tim
    let us | us0 <= 0  = 10000 -- fixme
           | otherwise = us0
    update us
    qlogLossTimerUpdated ldcc (tmi, Microseconds us) -- fixme tmi
  where
    update us = do
        mgr <- getSystemTimerManager
        key <- registerTimeout mgr us (onLossDetectionTimeout ldcc)
        mk <- atomicModifyIORef' timerKey (Just key,)
        forM_ mk $ unregisterTimeout mgr
        writeIORef timerInfo $ Just tmi

----------------------------------------------------------------

setLossDetectionTimer :: LDCC -> EncryptionLevel -> IO ()
setLossDetectionTimer ldcc@LDCC{..} lvl0 = do
    mtl <- getLossTimeAndSpace ldcc
    case mtl of
      Just (earliestLossTime,lvl) -> do
          when (lvl0 == lvl) $ do
              -- Time threshold loss detection.
              let tmi = TimerInfo earliestLossTime lvl LossTime
              updateLossDetectionTimer ldcc tmi
      Nothing -> do
          -- See beforeAntiAmp
          CC{..} <- readTVarIO recoveryCC
          validated <- peerCompletedAddressValidation ldcc
          if numOfAckEliciting <= 0 && validated then
              -- There is nothing to detect lost, so no timer is
              -- set. However, we only do this if the peer has
              -- been validated, to prevent the server from being
              -- blocked by the anti-amplification limit.
              cancelLossDetectionTimer ldcc
            else do
              -- Determine which PN space to arm PTO for.
              mx <- getPtoTimeAndSpace ldcc
              case mx of
                Nothing -> return ()
                Just (ptoTime, lvl) -> do
                    when (lvl0 == lvl) $ do
                        let tmi = TimerInfo ptoTime lvl PTO
                        updateLossDetectionTimer ldcc tmi

beforeAntiAmp :: LDCC -> IO ()
beforeAntiAmp ldcc = cancelLossDetectionTimer ldcc

----------------------------------------------------------------

-- The only time the PTO is armed when there are no bytes in flight is
-- when it's a client and it's unsure if the server has completed
-- address validation.
onLossDetectionTimeout :: LDCC -> IO ()
onLossDetectionTimeout ldcc@LDCC{..} = do
    alive <- getAlive ldcc
    when alive $ do
        mtmi <- readIORef timerInfo
        case mtmi of
          Nothing -> return ()
          Just tmi -> do
            let lvl = timerLevel tmi
            discarded <- getPacketNumberSpaceDiscarded ldcc lvl
            if discarded then
                updateWithNext ldcc
              else
                lossTimeOrPTO lvl tmi
  where
    lossTimeOrPTO lvl tmi = do
        qlogLossTimerExpired ldcc
        case timerType tmi of
          LossTime -> do
              -- Time threshold loss Detection
              lostPackets <- detectAndRemoveLostPackets ldcc lvl
              lostPackets' <- mergeLostCandidatesAndClear ldcc lostPackets
              when (null lostPackets') $ qlogDebug ldcc $ Debug "onLossDetectionTimeout: null"
              onPacketsLost ldcc lostPackets'
              retransmit ldcc lostPackets'
              setLossDetectionTimer ldcc lvl
          PTO -> do
              CC{..} <- readTVarIO recoveryCC
              if bytesInFlight > 0 then do
                  -- PTO. Send new data if available, else retransmit old data.
                  -- If neither is available, send a single PING frame.
                  sendPing ldcc lvl
                else do
                  -- Client sends an anti-deadlock packet: Initial is padded
                  -- to earn more anti-amplification credit,
                  -- a Handshake packet proves address ownership.
                  validated <- peerCompletedAddressValidation ldcc
                  when validated $ qlogDebug ldcc $ Debug "onLossDetectionTimeout: RTT1"
                  lvl' <- getEncryptionLevel ldcc -- fixme
                  sendPing ldcc lvl'

              metricsUpdated ldcc $
                  atomicModifyIORef'' recoveryRTT $
                      \rtt -> rtt { ptoCount = ptoCount rtt + 1 }
              setLossDetectionTimer ldcc lvl
