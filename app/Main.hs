module Main where
import Executor

main :: IO()
main = do
    execPlaybook AnsiblePlaybook { executionPath = "ansible-example", playbookName = "pb.yml", targetLimit = "", executeTags = "" }

    return ()
