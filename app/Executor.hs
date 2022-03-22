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
{-# LANGUAGE OverloadedStrings #-}

module Executor(AnsiblePlaybook(..), execPlaybook) where

import Ansible
import Sock
import Config
import DatabaseUtil

import Foreign.C.Types
import Foreign.C.String

import Control.Concurrent.Async (async, poll, waitCatch)
import Control.Monad
import Control.Monad.Logger (MonadLogger, LoggingT, runLoggingT, logInfoNS, logDebugNS)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (Reader, ReaderT, runReaderT, ask)

import Data.Maybe
import Data.Text(pack, Text)

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

data Callback = CallbackResult AnsibleRunnerResult
              | CallbackStart AnsibleRunnerStart
              | Callback AnsibleEvent
              | CallbackOther

-- TODO put in /run
sockPath = executorSockPath

writeToDatabase :: Callback -> ReaderT (ConnectionPool, RunId, Socket) IO ()
writeToDatabase (CallbackResult arr) = do
            (pool, rid, _) <- ask
            liftIO $ void $ addEvent (Event (taskARR arr) (taskIdARR arr)
                (playARR arr) (playIdARR arr) (hostARR arr) rid (is_changed arr)
                (is_skipped arr) (is_failed arr) (is_unreachable arr) (ignore_errors arr)
                (is_item arr) (item arr) "Output not implemented" ) pool
writeToDatabase _ = return ()

-- | Determine, which callback event was recieved
processAnsibleEvent :: String -> String -> Callback
processAnsibleEvent e s = case e of
        "task_runner_result" -> CallbackResult (decodeJSON s :: AnsibleRunnerResult)
        "task_runner_start"  -> CallbackOther-- CallbackStart  (decodeJSON s :: AnsibleRunnerStart)
        _ -> CallbackOther

processAnsibleCallbacks :: ReaderT (ConnectionPool, RunId, Socket) IO ()
processAnsibleCallbacks = forever $ do
    (_, _, sock) <- ask

    callbackRaw <- liftIO $ readSocket sock
    writeToDatabase $ processAnsibleEvent (event (decodeJSON callbackRaw :: AnsibleEvent)) callbackRaw

-- | execute Ansible Playbook defined by AnsiblePlaybook type,
-- write results to database set by pool under RunID rid
-- Return: True if run was OK, False otherwise
execPlaybook :: ConnectionPool -> RunId -> AnsiblePlaybook -> (LoggingT IO) Bool
execPlaybook pool rid pb = do
    logInfo "OwO"
    liftIO $ do
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

logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoNS "Executor" . pack

logDebug :: MonadLogger m => String -> m ()
logDebug = logDebugNS "Executor" . pack
