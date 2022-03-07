module Sock where
import Control.Monad (forever)

import Network.Socket
import Network.Socket.ByteString (recv)
import Data.ByteString.Internal

readLen :: Int
readLen = 2048

createBindSocket :: String -> IO Socket
createBindSocket s = do
        sock <- socket AF_UNIX Datagram 0
        bind sock $ SockAddrUnix s
        return sock

closeSocket :: Socket -> IO()
closeSocket = close

readForever :: Socket -> IO()
readForever s = forever $ do
        print =<< recv s 2048

readSocket :: Socket -> IO [Char]
readSocket s = do
    ret <- recv s 2048
    return $ unpackChars ret
