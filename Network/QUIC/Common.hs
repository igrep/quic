module Network.QUIC.Common where

import qualified Network.Socket as NS

import Network.QUIC.Connection
import Network.QUIC.Parameters
import Network.QUIC.Types

import Data.Foldable (foldMap)
import Foreign.Storable (peekByteOff)
import Foreign.Ptr (Ptr)
import Numeric (showHex)
import Data.Word (Word8)

----------------------------------------------------------------

data ConnRes = ConnRes Connection AuthCIDs (IO ())

connResConnection :: ConnRes -> Connection
connResConnection (ConnRes conn _ _) = conn

defaultPacketSize :: NS.SockAddr -> Int
defaultPacketSize NS.SockAddrInet6{} = defaultQUICPacketSizeForIPv6
defaultPacketSize _                  = defaultQUICPacketSizeForIPv4

maximumPacketSize :: NS.SockAddr -> Int
maximumPacketSize NS.SockAddrInet6{} = 1500 - 40 - 8 -- fixme
maximumPacketSize _                  = 1500 - 20 - 8 -- fixme

printBuf :: String -> Ptr Word8 -> Int -> IO ()
printBuf hd buf siz = putStrLn . (hd ++) =<< inHex
  where
    inHex :: IO String
    inHex = foldMap f [0 .. siz - 1]
    f offset = do
        w <- peekByteOff buf offset
        if (w :: Word8) < 16
          then return $ '0' : showHex w " "
          else return $ showHex w " "
