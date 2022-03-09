{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Executor where

import Ansible
import Sock

import Foreign.C.Types
import Foreign.C.String

import Control.Concurrent.Async
import Control.Monad

import System.Posix.Env
import System.Directory

import Text.JSON
import Text.JSON.Generic

import Text.Printf

-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON.html#t:JSON
-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON.html
-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON-Generic.html

data AnsiblePlaybook = AnsiblePlaybook
    { path :: String
    , name :: String
    , tags :: String
    , limit :: String
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

newtype AnsibleEvent = AnsibleEvent
    { event :: String
    } deriving (Show, Data)

-- TODO put in /run
sockPath = "/tmp/hansible.sock"

notifyDatabase :: AnsibleRunnerResult -> IO ()
notifyDatabase arr = do
        printf "Result: %s, F: %s UR: %s SK: %s\n"
            (taskARR arr) (show $ is_failed arr)
            (show $ is_unreachable arr) (show $ is_skipped arr)

notifyScheduler :: AnsibleRunnerStart -> IO ()
notifyScheduler ars = do
        printf "Start: %s, Host: %s\n"
            (taskARS ars) (hostARS ars)

processAnsibleEvent :: String -> String -> IO ()
processAnsibleEvent "task_runner_result" s =
        notifyDatabase (decodeJSON s :: AnsibleRunnerResult)
processAnsibleEvent "task_runner_start" s =
        notifyScheduler (decodeJSON s :: AnsibleRunnerStart)
processAnsibleEvent e s = return ()

exec :: AnsiblePlaybook -> IO ()
exec pb = do
        putEnv $ "HANSIBLE_OUTPUT_SOCKET=" ++ sockPath

        sock <- createBindSocket sockPath
        let run = True
        pb <- async $ ansiblePlaybook (path pb) (name pb) (limit pb) (tags pb)
        as <- async $ forever $ do
            callbackRaw <- readSocket sock
            let callbackAE = decodeJSON callbackRaw :: AnsibleEvent
            processAnsibleEvent (event callbackAE) callbackRaw

        ret <- wait pb
        cancel as

        closeSocket sock
        removeFile sockPath

        printf "Ansible return: %i" ret
