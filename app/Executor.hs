{-# LANGUAGE DeriveDataTypeable #-}
module Executor where

import Ansible
import Sock

import Foreign.C.Types
import Foreign.C.String

import Control.Concurrent.Async
import Control.Monad (forever)

import System.Posix.Env

import Text.JSON
import Text.JSON.Generic

-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON.html#t:JSON
-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON.html
-- https://hackage.haskell.org/package/json-0.10/docs/Text-JSON-Generic.html

--data AnsibleRunnerResult = AnsibleRunnerResult
--    { event :: String
--    , playbook :: String
--    , playbook_id :: Int
--    , play :: String
--    , play_id :: Int
--    , task :: String
--    , task_id :: Int
--    , host :: String
--    , is_changed :: Bool
--    , is_skipped :: Bool
--    , is_failed :: Bool
--    , is_unreachable :: Bool
--    , ignore_errors :: Bool
--    , delegate :: Bool
--    , delegate_host :: String
--    , is_item :: Bool
--    , item :: String
--    } deriving (Show, Data)

data AnsibleEvent = AnsibleEvent
    { event :: String 
    } deriving (Show, Data)

aaa :: String
aaa = "{\"event\": \"task_runner_result\", \"playbook\": \"pb.yml\", \"playbook_id\": 1, \"play\": \"Test\", \"play_id\": 1, \"task\": \"Test\", \"task_id\": 2, \"host\": \"localhost\", \"is_changed\": false, \"is_skipped\": false, \"is_failed\": false, \"is_unreachable\": false, \"ignore_errors\": false, \"delegate\": false, \"delegate_host\": \"\", \"is_item\": true, \"item\": \"This\"}"

sockpath = "/tmp/socket"

exec :: IO ()
exec = do
        putEnv $ "HANSIBLE_OUTPUT_SOCKET=" ++ sockpath

        sock <- createBindSocket sockpath

        pb <- async $ ansiblePlaybook "ansible-example" "pb.yml" "limit" "tag"
        -- as <- async $ readForever sock
        as <- async $ forever $ do
            val <- readSocket sock
            print( decodeJSON val :: AnsibleEvent )

        ret <- wait pb

        -- print(decodeJSON aaa :: AnsibleEvent)
        -- print(decodeJSON aaa :: AnsibleEvent)
        -- let a = decodeJSON aaa :: AnsibleRunnerResult

--        print a

        closeSocket sock

        print ret
