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

import Data.Maybe

import System.Posix.Env
import System.Directory

import Text.JSON
import Text.JSON.Generic
import Text.Printf

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

notifyDatabase :: AnsibleRunnerResult -> ConnectionPool -> RunId -> IO ()
notifyDatabase arr pool rid = do
        runSqlPool (insert $ Event (taskARR arr) (taskIdARR arr) (playARR arr)
            (playIdARR arr) (hostARR arr) rid (is_changed arr) (is_skipped arr)
            (is_failed arr) (is_unreachable arr) (is_item arr) (item arr)
            "Output not implemented") pool

        printf "Result: %s, F: %s UR: %s SK: %s\n"
            (taskARR arr) (show $ is_failed arr)
            (show $ is_unreachable arr) (show $ is_skipped arr)

notifyScheduler :: AnsibleRunnerStart -> IO ()
notifyScheduler ars = do
        printf "Start: %s, Host: %s\n"
            (taskARS ars) (hostARS ars)

processAnsibleEvent :: String -> String -> ConnectionPool -> RunId -> IO ()
processAnsibleEvent "task_runner_result" s pool rid =
        notifyDatabase (decodeJSON s :: AnsibleRunnerResult) pool rid
processAnsibleEvent "task_runner_start" s pool rid =
        notifyScheduler (decodeJSON s :: AnsibleRunnerStart)
processAnsibleEvent e s p r = return ()

execPlaybook :: ConnectionPool -> RunId -> AnsiblePlaybook -> IO Bool
execPlaybook pool rid pb = do
        putEnv $ "HANSIBLE_OUTPUT_SOCKET=" ++ sockPath

        sock <- createBindSocket sockPath

        pb <- async $ ansiblePlaybook (executionPath pb) (playbookName pb) (targetLimit pb) (executeTags pb)
        as <- async $ forever $ do
            -- Poll pb-thread
            callbackRaw <- readSocket sock
            let callbackAE = decodeJSON callbackRaw :: AnsibleEvent
            processAnsibleEvent (event callbackAE) callbackRaw pool rid

        ret <- wait pb

        closeSocket sock
        -- We want to catch the "Bad File descriptor" of the previously
        -- closed socket
        poll as >>= \x -> when (isNothing x) $ void(waitCatch as)

        removeFile sockPath

        return $ ret==0
