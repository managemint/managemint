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

module Executor(AnsiblePlaybook(..), ExecutorStatus(..), execPlaybook) where

import Ansible
import Sock
import Config
import DatabaseUtil
import LoggerUtil

import Foreign.C.Types
import Foreign.C.String

import Control.Concurrent.Async (async, poll, waitCatch, wait)
import Control.Concurrent (threadDelay)

import Control.Monad (when, unless, void)
import Control.Monad.RWS (RWST, execRWST, ask, liftIO, lift, get, gets, modify, put)

import Control.Lens.Operators ((.=))
import Control.Lens.Combinators (_3, _4)

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

data ExecutorStatus = ExecutorNoErr
                    | ExecutorInternalError
                    | ExecutorExternalError
                    deriving(Show, Eq)

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

type ExecutorMT = RWST (ConnectionPool, RunId, Handle) () Bool

-- | Determine, which callback event was recieved, Decode the string
processAnsibleEvent :: String -> String -> Callback
processAnsibleEvent e s = case e of
        "task_runner_result" -> CallbackResult (decodeJSON s :: AnsibleRunnerResult)
        "task_runner_start"  -> CallbackOther -- CallbackStart  (decodeJSON s :: AnsibleRunnerStart)
        "end"                -> CallbackEnd
        _                    -> CallbackOther

handleCallback :: Callback -> ExecutorMT (LoggingT IO) ()
handleCallback (CallbackResult arr) = do
            (pool, rid, _ ) <- ask
            logDebug $ "Callback status for '" ++ taskARR arr ++ "' on '" ++ hostARR arr ++ "'"
            liftIO $ void $ addEvent (Event (taskARR arr) (taskIdARR arr)
                (playARR arr) (playIdARR arr) (hostARR arr) rid (is_changed arr)
                (is_skipped arr) (is_failed arr) (is_unreachable arr) (ignore_errors arr)
                (is_item arr) (item arr) "Output not implemented" (delegate arr) (delegate_host arr) ) pool
handleCallback CallbackEnd = put True
handleCallback _ = return ()

-- | Try to process Ansible callbacks as long as iorb is true
processAnsibleCallbacks :: IORef Bool -> ExecutorMT (LoggingT IO) ()
processAnsibleCallbacks iorb = do
    (_, _, handle) <- ask

    maybe
        (liftIO $ threadDelay 10000) -- No Data was recieved
        (\s -> handleCallback $ processAnsibleEvent (event (decodeJSON s :: AnsibleEvent)) s)
        =<< liftIO (maybeReadHandle handle)

    liftIO (readIORef iorb) >>=
        \b -> when b $ processAnsibleCallbacks iorb

determineExecutorStatus :: Int -> Bool -> ExecutorStatus
determineExecutorStatus 0 False = ExecutorInternalError
determineExecutorStatus 0 True  = ExecutorNoErr
determineExecutorStatus _ _     = ExecutorExternalError

-- | execute Ansible Playbook defined by AnsiblePlaybook type,
-- write results to database set by pool under RunID rid
-- Return: True if run was OK, False otherwise
execPlaybook :: ConnectionPool -> RunId -> AnsiblePlaybook -> (LoggingT IO) ExecutorStatus
execPlaybook pool rid pb = do
    logInfo $ "RUN #" ++ show (keyToInt rid) ++ ": Executing " ++ show pb

    liftIO $ putEnv $ "MANAGEMINT_OUTPUT_SOCKET=" ++ executorSockPath
    handle <- liftIO $ handleFromSocket =<< createBindSocket executorSockPath

    continue <- liftIO $ newIORef True
    cb  <- asyncWithLogger (execRWST (processAnsibleCallbacks continue) (pool, rid, handle) False)

    logDebug "Spawned async callback processor. Now running Ansible"
    ret <- liftIO $ ansiblePlaybook (executionPath pb) (playbookName pb) (targetLimit pb) (executeTags pb)

    logDebug $ "Ansible returned with " ++ show ret ++ ". Waiting a bit for all data to arrive..."

    -- Wait for all data to arrive at socket. 2s seems to work
    liftIO $ threadDelay 2000000
    logDebug "Trying to stop callback processor"
    liftIO $ writeIORef continue False

    (hasEnded,_) <- liftIO $ wait cb
    logDebug "Callback processor stopped"
    unless hasEnded $ logWarn "No Stats-Callback was recieved. Ansible did NOT terminate gracefully."

    liftIO $ closeHandle handle
    liftIO $ removeFile executorSockPath

    logInfo "Executor finished gracefully."
    return $ determineExecutorStatus ret hasEnded


logInfo :: MonadLogger m => String -> m ()
logInfo = hLogInfo "Executor"

logDebug :: MonadLogger m => String -> m ()
logDebug = hLogDebug "Executor"

logWarn :: MonadLogger m => String -> m ()
logWarn = hLogWarn "Executor"
