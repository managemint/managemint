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

import Control.Concurrent.Async (async, poll, waitCatch, wait)
import Control.Concurrent (threadDelay)

import Control.Monad (when, unless, void)
import Control.Monad.Logger (MonadLogger, LoggingT, runLoggingT, logInfoNS, logDebugNS, logWarnNS)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (Reader, ReaderT, runReaderT, ask)
import Control.Monad.Trans.State (State, StateT, runStateT, get, put, modify)

import Control.Lens.Operators ((.=))
import Control.Lens.Combinators (_4)

import Data.Maybe
import Data.Text(pack, Text)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

import System.Posix.Env
import System.Directory

import Text.JSON
import Text.JSON.Generic
import Text.Printf

import Network.Socket (Socket)

import Database.Persist.MySQL (ConnectionPool)

import GHC.IO.Handle (Handle)

-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON.html#t:JSON
-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON.html
-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON-Generic.html

data AnsiblePlaybook = AnsiblePlaybook
    { executionPath :: String
    , playbookName :: String
    , executeTags :: String
    , targetLimit :: String
    } deriving (Data)

instance Show AnsiblePlaybook where
    show p = "Playbook :: path: " ++ executionPath p ++ " playbook: " ++ playbookName p

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
              | CallbackEnd
              | CallbackOther

type ExecutorMT = StateT (ConnectionPool, RunId, Handle, Bool)

-- | Determine, which callback event was recieved, Decode the string
processAnsibleEvent :: String -> String -> Callback
processAnsibleEvent e s = case e of
        "task_runner_result" -> CallbackResult (decodeJSON s :: AnsibleRunnerResult)
        "task_runner_start"  -> CallbackOther -- CallbackStart  (decodeJSON s :: AnsibleRunnerStart)
        "end"                -> CallbackEnd
        _                    -> CallbackOther

handleCallback :: Callback -> ExecutorMT IO ()
handleCallback (CallbackResult arr) = do
            (pool, rid, _, _) <- get
            liftIO $ void $ addEvent (Event (taskARR arr) (taskIdARR arr)
                (playARR arr) (playIdARR arr) (hostARR arr) rid (is_changed arr)
                (is_skipped arr) (is_failed arr) (is_unreachable arr) (ignore_errors arr)
                (is_item arr) (item arr) "Output not implemented" ) pool
handleCallback CallbackEnd = _4 .= True
handleCallback _ = return ()

processAnsibleCallbacks :: IORef Bool -> ExecutorMT IO Bool
processAnsibleCallbacks iorb = do
    liftIO $ threadDelay 10000
    (_, _, handle, _) <- get

    callbackRaw <- liftIO $ maybeReadHandle handle
    case callbackRaw of
      (Just s) -> do
          handleCallback $ processAnsibleEvent (event (decodeJSON s :: AnsibleEvent)) s
      Nothing  -> return ()

    continue <- liftIO $ readIORef iorb
    if continue
      then processAnsibleCallbacks iorb
      else do
        (_, _, _, ret) <- get
        return ret

-- | execute Ansible Playbook defined by AnsiblePlaybook type,
-- write results to database set by pool under RunID rid
-- Return: True if run was OK, False otherwise
execPlaybook :: ConnectionPool -> RunId -> AnsiblePlaybook -> (LoggingT IO) Bool
execPlaybook pool rid pb = do
    logInfo $ "Executing " ++ show pb

    liftIO $ putEnv $ "HANSIBLE_OUTPUT_SOCKET=" ++ executorSockPath
    handle <- liftIO $ handleFromSocket =<< createBindSocket executorSockPath

    continue <- liftIO $ newIORef True
    cb  <- liftIO $ async $ runStateT (processAnsibleCallbacks continue) (pool, rid, handle, False)

    logDebug "Spawned async callback processor. Now running Ansible"
    ret <- liftIO $ ansiblePlaybook (executionPath pb) (playbookName pb) (targetLimit pb) (executeTags pb)

    logDebug $ "Ansible returned with " ++ show ret ++ ". Waiting a bit for all data to arrive..."

    -- Wait for all data to arrive at socket
    liftIO $ threadDelay 1000000
    logDebug "Trying to stop callback processor"
    liftIO $ writeIORef continue False

    -- TODO Check return of StateT??
    (hasEnded, _) <- liftIO $ wait cb
    logDebug "Callback processor stopped"
    unless hasEnded $ logWarn "No Stats-Callback was recieved. Ansible did NOT terminate gracefully."

    liftIO $ closeHandle handle
    liftIO $ removeFile executorSockPath

    logInfo "Executor finished gracefully."

    return $ ret==0

logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoNS "Executor" . pack

logDebug :: MonadLogger m => String -> m ()
logDebug = logDebugNS "Executor" . pack

logWarn :: MonadLogger m => String -> m ()
logWarn = logWarnNS "Executor" . pack
