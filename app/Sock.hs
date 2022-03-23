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

import Control.Monad (forever, when)

import Network.Socket (Socket, socket, bind, close, Family(..), SocketType(..), SockAddr(..), socketToHandle)
import Network.Socket.ByteString (recv)

import Data.ByteString.Internal (unpackChars)
import Data.ByteString (hGetNonBlocking, empty, null)

import GHC.IO.Handle (Handle, hClose)
import System.IO (IOMode(..))

-- | Length to read from Socket
readLen :: Int
readLen = sockReadLen

-- | Create and bind a UNIX socket at path s
createBindSocket :: String -> IO Socket
createBindSocket s = do
        sock <- socket AF_UNIX Datagram 0
        bind sock $ SockAddrUnix s
        return sock

handleFromSocket :: Socket -> IO Handle
handleFromSocket = flip socketToHandle ReadWriteMode

maybeReadHandle :: Handle -> IO (Maybe String)
maybeReadHandle h = do
    bs <- hGetNonBlocking h readLen

    if bs == empty
       then return Nothing
       else return $ Just $ unpackChars bs

closeHandle :: Handle -> IO()
closeHandle = hClose

closeSocket :: Socket -> IO()
closeSocket = close

readForever :: Socket -> IO()
readForever s = forever $ do
        print =<< recv s readLen

-- | Read from a Socket into a String
readSocket :: Socket -> IO [Char]
readSocket s = do
    ret <- recv s readLen
    return $ unpackChars ret
