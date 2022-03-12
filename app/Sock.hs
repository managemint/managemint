{- app/Sock.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

module Sock where
import Config
import Control.Monad (forever)

import Network.Socket
import Network.Socket.ByteString (recv)
import Data.ByteString.Internal

readLen :: Int
readLen = sockReadLen

createBindSocket :: String -> IO Socket
createBindSocket s = do
        sock <- socket AF_UNIX Datagram 0
        bind sock $ SockAddrUnix s
        return sock

closeSocket :: Socket -> IO()
closeSocket = close

readForever :: Socket -> IO()
readForever s = forever $ do
        print =<< recv s readLen

readSocket :: Socket -> IO [Char]
readSocket s = do
    ret <- recv s readLen
    return $ unpackChars ret
