module Main where

import Ansible
import Sock

import Foreign.C.Types
import Foreign.C.String

import Control.Concurrent.Async

import System.Posix.Env

sockpath = "/tmp/socket"

main :: IO ()
main = do
        putEnv $ "HANSIBLE_OUTPUT_SOCKET=" ++ sockpath

        sock <- createBindSocket sockpath

        pb <- async $ ansiblePlaybook "../ansible" "pb.yml" "limit" "tag"
        as <- async $ readForever sock

        ret <- wait pb

        closeSocket sock

        print ret
