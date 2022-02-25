module Scheduler where

import Ansible
import Control.Concurrent
import GHC.IO.Device (IODevice(isTerminal))
import Data.IORef
import Data.Functor ((<&>))
import Data.Time.Clock
import Data.List (delete)

-- TODO: [ ] If failcount exeeds a certain threshold, block the job

data Playbook = Playbook {_name :: String}
    deriving (Eq)

-- |A Job is an ansbile playbook which should be executed at a certain date with some optionale arguments 
-- |and a fail counter
data Job = Job {_timeDue :: UTCTime , _playbook :: Playbook, _optArgs:: [String], _failCount :: Int}
    deriving (Eq)
type Jobs = [Job]

instance Ord Job where
    compare j1 j2 = compare (_timeDue j1) (_timeDue j2)

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _      = Nothing

insertSort :: Ord a => a -> [a] -> [a]
insertSort x [] = [x]
insertSort x xs
    | x <  m    = insertSort x left ++ right  -- TODO (++) is not very efficient
    | x >  m    = left ++ insertSort x right
    | otherwise = left ++ x:right
    where
        mid = length xs `div` 2
        (left,right@(m:_)) = splitAt mid xs

-- |Merges two jobs and sorts them
-- |Assumes that the second job list ist sorted
mergeJobs :: Jobs -> Jobs -> Jobs
mergeJobs js sjs = foldl (flip insertJob) sjs js

-- |Inserts a job in the jobs queue, keeps the sorting
insertJob :: Job -> Jobs -> Jobs
insertJob = insertSort

removeJob :: Job -> Jobs -> Jobs
removeJob = delete

readPlaybookRepo :: IO [Playbook]
readPlaybookRepo = undefined

calculateNextOccurence :: Playbook -> IO Job
calculateNextOccurence = undefined

-- |Runs the ansible job and if the job is recurring or fails, returns the next instance of that job
-- |Increases fail count if the job fails
executeJob :: Job -> IO (Maybe Job)
executeJob = undefined

schedule :: IORef Jobs -> IO ()
schedule rjobs = do
    job <- readIORef rjobs <&> safeHead
    job >>= Just . modifyIORef rjobs . removeJob `seq` return ()  -- I guess this works?

    currentTime <- getCurrentTime
    job' <- case job of
                 Just j -> if _timeDue j >= currentTime then executeJob j else return job
                 _      -> return job

    jobsConfig <- readPlaybookRepo >>= mapM calculateNextOccurence
    let newJobs = case job' of
                       Just f -> insertJob f jobsConfig
                       _      -> jobsConfig
    modifyIORef rjobs $ mergeJobs newJobs

    threadDelay 1000000 -- Sleep for one second
    schedule rjobs
