{-# LANGUAGE TemplateHaskell #-}

module Scheduler where

import Ansible
import Control.Concurrent
import GHC.IO.Device (IODevice(isTerminal))
import Data.IORef
import Data.Functor ((<&>))
import Data.Time.Clock
import Data.List (delete)
import Data.Maybe (isJust, catMaybes)
import Control.Monad (when)
import Control.Lens

-- TODO: [ ] If failcount exeeds a certain threshold, block the job
--       [ ] Playbook id is the absolut path

data Playbook = Playbook {_name :: String, _playbookID :: Int}
    deriving (Eq)

-- |A Job is an ansbile playbook which should be executed at a certain date with some optionale arguments 
-- |and a fail counter
data Job = Job {_timeDue :: UTCTime, _playbook :: Playbook, _optArgs:: [String], _failCount :: Int, _system :: Bool}
    deriving (Eq)
type Jobs = [Job]
--            Due    System User
data Queue = Queue {_dueJobs :: Jobs, _systemJobs :: Jobs, _userJobs :: Jobs}

makeLenses ''Playbook
makeLenses ''Job
makeLenses ''Queue

instance Ord Job where
    compare j1 j2 = compare (_timeDue j1) (_timeDue j2)

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _      = Nothing

insertSorted :: Ord a => [a] -> a -> [a]
insertSorted []     x = [x]
insertSorted (y:ys) x = case compare x y of
                             GT -> y : insertSorted ys x
                             _  -> x:y:ys

-- |Inserts the items from the first job list into the second
--  Assumes that the first list is sorted
mergeJobs :: Jobs -> Jobs -> Jobs
mergeJobs = foldl insertJob

-- |Inserts a job in the jobs queue, keeps the sorting
insertJob :: Jobs -> Job -> Jobs
insertJob = insertSorted

equivJob :: Job -> Job -> Bool
equivJob j1 j2 = j1^.playbook.playbookID == j2^.playbook.playbookID

-- |Updates a job in the queque
updateJob :: (Job -> Job -> Bool) -> Jobs -> Job -> Jobs
updateJob _ []     j = []
updateJob f (x:xs) j = if j `f` x then j:xs else x : updateJob f xs j

-- |Updates the first job list with the elements of the second
--  Assumes that the first list is sorted
updateJobs :: Jobs -> Jobs -> Jobs
updateJobs = foldl $ updateJob equivJob

removeJob :: Job -> Jobs -> Jobs
removeJob = delete

-- |Given a time, splits a job list in two parts; the due and upcoming ones
splitJobsDue :: UTCTime -> Jobs -> (Jobs, Jobs)
splitJobsDue time = span $ \job -> (job^.timeDue) <= time

readPlaybookRepo :: IO Jobs
readPlaybookRepo = undefined

readDatabaseQueue :: IO Jobs
readDatabaseQueue = undefined

calculateNextOccurence :: Playbook -> IO Job
calculateNextOccurence = undefined

-- |Runs the ansible job and if the job is recurring or failed for less than 3 times, returns the next instance of that job
--  Increases fail count if the job fails
executeJob :: Job -> IO (Maybe Job)
executeJob = undefined

schedule :: IORef Queue -> IO ()
schedule queueRef = do
    -- [x] Update system and user queue
    -- [x] Pop due jobs
    -- [x] Add due jobs to due queue (push)
    -- [x] Pop head of due queue
    -- [x] Execute the job
    -- [x] Maybe add jobs to q again
    readPlaybookRepo  >>= \repoJobs     -> modifyIORef queueRef (& systemJobs %~ (`updateJobs` repoJobs))
    readDatabaseQueue >>= \databaseJobs -> modifyIORef queueRef (& userJobs   %~ (`mergeJobs`  databaseJobs))

    -- Refactor perhaps with lenses
    queue <- readIORef queueRef
    time <- getCurrentTime
    let (ds,us) = splitJobsDue time (queue^.systemJobs)
    let (du,uu) = splitJobsDue time (queue^.userJobs)
    writeIORef queueRef $ Queue (queue^.dueJobs ++ mergeJobs ds du) us uu

    maybeJob  <- (\q -> safeHead (q^.dueJobs)) <$> readIORef queueRef
    maybeJob' <- maybe (return Nothing) executeJob maybeJob

    q  <- readIORef queueRef
    mapM executeJob (q^.dueJobs) >>= (\x -> writeIORef queueRef (q & dueJobs .~ x)) . catMaybes
