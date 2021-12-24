module Network.QUIC.Types.Queue where

import Control.Concurrent.STM

import Network.QUIC.Types.Packet

newtype RecvQ = RecvQ (TQueue ReceivedPacket)

newRecvQ :: IO RecvQ
newRecvQ = RecvQ <$> newTQueueIO

readRecvQ :: RecvQ -> IO ReceivedPacket
readRecvQ (RecvQ q) = atomically $ readTQueue q

writeRecvQ :: RecvQ -> ReceivedPacket -> IO ()
writeRecvQ (RecvQ q) x = (putStrLn $ "writeRecvQ " ++ show (rpEncryptionLevel x)) >> (atomically $ writeTQueue q x)

prependRecvQ :: RecvQ -> ReceivedPacket -> STM ()
prependRecvQ (RecvQ q) = unGetTQueue q
