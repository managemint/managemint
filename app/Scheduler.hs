{-# LANGUAGE TemplateHaskell #-}

module Scheduler where

import Ansible
import Control.Concurrent
import GHC.IO.Device (IODevice(isTerminal))
import Data.IORef
import Data.Functor ((<&>))
import Data.Time.Clock
import Data.List (delete, intercalate)
import Data.Maybe (isJust, catMaybes)
import Control.Monad (when)
import Control.Lens
import qualified Database.Persist.MySQL as MySQL

data Playbook = Playbook {_name :: String, _playbookID :: Int}
    deriving (Eq)

-- |A Job is an ansbile playbook which should be executed at a certain date with some optionale arguments 
--  and a fail counter
data Job = Job {_timeDue :: UTCTime, _playbook :: Playbook, _optArgs:: [String], _failCount :: Int, _system :: Bool}
    deriving (Eq)
type Jobs = [Job]
data Queue = Queue {_dueJobs :: Jobs, _systemJobs :: Jobs, _userJobs :: Jobs}

makeLenses ''Playbook
makeLenses ''Job
makeLenses ''Queue

instance Ord Job where
    compare j1 j2 = compare (_timeDue j1) (_timeDue j2)

connectionInfo :: MySQL.ConnectInfo
connectionInfo = MySQL.defaultConnectInfo 
    { MySQL.connectHost     = "mdbtest-11.my.cum.re"
    , MySQL.connectUser     = "hansible"
    , MySQL.connectPassword = "AffqDbF2Vw5Aq7EHferw"
    , MySQL.connectDatabase = "hansible"
    }

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

-- |Inserts a job in the job list, keeps the sorting
insertJob :: Jobs -> Job -> Jobs
insertJob = insertSorted

-- |Updates a job in a job list
--  A Job is identified by its id
updateJob :: Jobs -> Job -> Jobs
updateJob []     j = []
updateJob (x:xs) j = if j `equiv` x then j:xs else x : updateJob xs j
    where equiv j1 j2 = j1^.playbook.playbookID == j2^.playbook.playbookID

-- |Updates the first job list with the elements of the second
--  Assumes that the first list is sorted
updateJobs :: Jobs -> Jobs -> Jobs
updateJobs = foldl updateJob

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

-- |Runs the ansible job and if the job is recurring or failed 3 times, returns the next instance of that job
--  Increases fail count if the job fails
executeJob :: Job -> IO (Maybe Job)
executeJob job = do
    ret <- ansiblePlaybook "../ansible" (job^.playbook.name) (intercalate ";" job^.optArgs) ""  -- TODO: Lookup how to join optional arguments
    if ret == 1 || job^.failCount == 3 then return Nothing else return $ Just $ job & failCount %~ (+1)

schedule :: IORef Queue -> IO ()
schedule queueRef = do
    time  <- getCurrentTime
    readPlaybookRepo  >>= \repoJobs     -> modifyIORef queueRef (& systemJobs %~ (`updateJobs` repoJobs))
    readDatabaseQueue >>= \databaseJobs -> modifyIORef queueRef (& userJobs   %~ (`mergeJobs`  databaseJobs))

    queue <- readIORef queueRef
    let (ds,us) = splitJobsDue time (queue^.systemJobs)
    let (du,uu) = splitJobsDue time (queue^.userJobs)
    let queueDueJobs = queue^.dueJobs ++ mergeJobs ds du

    failedJobs <- mapM executeJob queueDueJobs <&> catMaybes
    writeIORef queueRef $ Queue failedJobs us uu
