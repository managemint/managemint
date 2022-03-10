module Main where
import Executor

main :: IO()
main = do
    exec AnsiblePlaybook { path = "ansible-example", name = "pb.yml", limit = "", tags = "" }
    return ()
