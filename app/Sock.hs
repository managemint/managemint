module Sock where
import Control.Monad (forever)

import Network.Socket
import Network.Socket.ByteString (recv)

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
