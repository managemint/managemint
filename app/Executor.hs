{- app/Executor.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Executor(AnsiblePlaybook(..), execPlaybook) where

import Ansible
import Sock
import Config
import DatabaseUtil

import Foreign.C.Types
import Foreign.C.String

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)

import Data.Maybe

import System.Posix.Env
import System.Directory

import Text.JSON
import Text.JSON.Generic
import Text.Printf

import Network.Socket (Socket)

import Database.Persist.MySQL
import Database.Persist

-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON.html#t:JSON
-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON.html
-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON-Generic.html

data AnsiblePlaybook = AnsiblePlaybook
    { executionPath :: String
    , playbookName :: String
    , executeTags :: String
    , targetLimit :: String
    } deriving (Show, Data)

data AnsibleRunnerStart = AnsibleRunnerStart
    { playbook :: String
    , playbook_id :: Int
    , play :: String
    , play_id :: Int
    , task :: String
    , task_id :: Int
    , host :: String
    } deriving (Show, Data)

hostARS :: AnsibleRunnerStart -> String
hostARS = host

taskARS :: AnsibleRunnerStart -> String
taskARS = task

data AnsibleRunnerResult = AnsibleRunnerResult
    { playbook :: String
    , playbook_id :: Int
    , play :: String
    , play_id :: Int
    , task :: String
    , task_id :: Int
    , host :: String
    , is_changed :: Bool
    , is_skipped :: Bool
    , is_failed :: Bool
    , is_unreachable :: Bool
    , ignore_errors :: Bool
    , delegate :: Bool
    , delegate_host :: String
    , is_item :: Bool
    , item :: String
    } deriving (Show, Data)

hostARR :: AnsibleRunnerResult -> String
hostARR = host

taskARR :: AnsibleRunnerResult -> String
taskARR = task

taskIdARR :: AnsibleRunnerResult -> Int
taskIdARR = task_id

playARR :: AnsibleRunnerResult -> String
playARR = play

playIdARR :: AnsibleRunnerResult -> Int
playIdARR = play_id

newtype AnsibleEvent = AnsibleEvent
    { event :: String
    } deriving (Show, Data)

-- TODO put in /run
sockPath = executorSockPath

-- | Write Runner result to database
writeResult :: AnsibleRunnerResult -> ReaderT (ConnectionPool, RunId, Socket) IO ()
writeResult arr = do
            (pool, rid, _) <- ask
            liftIO $ void $ addEvent (Event (taskARR arr) (taskIdARR arr)
                (playARR arr) (playIdARR arr) (hostARR arr) rid (is_changed arr)
                (is_skipped arr) (is_failed arr) (is_unreachable arr) (ignore_errors arr)
                (is_item arr) (item arr) "Output not implemented" ) pool

-- | handle runner start, unused
writeStart :: AnsibleRunnerStart -> ReaderT (ConnectionPool, RunId, Socket) IO ()
writeStart ars = return ()

-- | Determine, which callback event was recieved
processAnsibleEvent :: String -> String -> ReaderT (ConnectionPool, RunId, Socket) IO ()
processAnsibleEvent e s = case e of
        "task_runner_result" -> writeResult (decodeJSON s :: AnsibleRunnerResult)
        "task_runner_start"  -> writeStart  (decodeJSON s :: AnsibleRunnerStart)
        _ -> return ()

processAnsibleCallbacks :: ReaderT (ConnectionPool, RunId, Socket) IO ()
processAnsibleCallbacks = forever $ do
    (_, _, sock) <- ask

    callbackRaw <- liftIO $ readSocket sock
    processAnsibleEvent (event (decodeJSON callbackRaw :: AnsibleEvent)) callbackRaw

-- | execute Ansible Playbook defined by AnsiblePlaybook type,
-- write results to database set by pool under RunID rid
-- Return: True if run was OK, False otherwise
execPlaybook :: ConnectionPool -> RunId -> AnsiblePlaybook -> IO Bool
execPlaybook pool rid pb = do
        putEnv $ "HANSIBLE_OUTPUT_SOCKET=" ++ sockPath
        sock <- createBindSocket sockPath

        cb  <- async $ runReaderT processAnsibleCallbacks (pool, rid, sock)
        ret <- ansiblePlaybook (executionPath pb) (playbookName pb) (targetLimit pb) (executeTags pb)

        closeSocket sock
        -- We want to catch the "Bad File descriptor" of the previously
        -- closed socket
        poll cb >>= \x -> when (isNothing x) $ void(waitCatch cb)

        removeFile sockPath

        return $ ret==0
