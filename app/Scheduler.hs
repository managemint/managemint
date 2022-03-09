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

-- Ich lese die User Jobs aus der Databse, erstelle Templates und darus Jobs die zu q hinzugefügt werden
-- Außerdem prüfe ich ob die Config Repo ein update hat
--   Ja   -> JobTemplate updaten und neue Instanzen bestimmen und damit die systemJobs ersetzen
--   Nein -> Nichts machen
-- Die fälligen user und systemjobs ausführen
-- Bei system jobs neue instanz hizufügen und falls fail failcount erhöhen (execute -> catMaybes -> calculateNextInstance)

data JobTemplate = JobTemplate {_jobName :: String, _scheduleFormat :: Schedule, _playbook :: String , _optArgs:: [String], _failCount :: Int}
    deriving (Eq)
-- A Job is an instance of a JobTemplate
data Job = Job {_timeDue :: UTCTime, _template :: JobTemplate}
    deriving (Eq)
type Jobs = [Job]
data Queue = Queue {_systemJobs :: Jobs, _userJobs :: Jobs}
data Schedule = Foo
    deriving (Eq)

makeLenses ''Job
makeLenses ''JobTemplate
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
--Assumes that the first list is sorted
mergeJobs :: Jobs -> Jobs -> Jobs
mergeJobs = foldl insertJob

-- |Inserts a job in the job list, keeps the sorting
insertJob :: Jobs -> Job -> Jobs
insertJob = insertSorted

removeJob :: Job -> Jobs -> Jobs
removeJob = delete

-- |Given a time, splits a job list in two parts; the due and upcoming ones
splitJobsDue :: UTCTime -> Jobs -> (Jobs, Jobs)
splitJobsDue time = span $ \job -> (job^.timeDue) <= time

createUserJobs :: IO Jobs
createUserJobs = undefined

configRepoUpdate :: IO Bool
configRepoUpdate = undefined

readConfigRepo :: IO [JobTemplate]
readConfigRepo = undefined

calculateNextInstance :: JobTemplate -> IO Job
calculateNextInstance = undefined

calculateNextInstances :: [JobTemplate] -> IO Jobs
calculateNextInstances = mapM calculateNextInstance

-- |Runs the ansible job and if the job is recurring and failed less than 3 times, returns the next instance of that job
--Increases fail count if the job fails
executeJob :: Job -> IO (Maybe Job)
executeJob job = do
    ret <- ansiblePlaybook "../ansible" (job^.template.playbook) (intercalate "," (job^.template.optArgs)) ""  -- TODO: Lookup how to join optional arguments
    if ret == 1 || job^.template.failCount == 3 then return Nothing else return $ Just $ job & template.failCount %~ (+1)

schedule :: IORef Queue -> [JobTemplate] -> IO ()
schedule queueRef jobTempls = do
    createUserJobs >>= \u -> modifyIORef queueRef (& userJobs %~ (`mergeJobs` u))

    configRepoUpdate >>= \b -> when b $ do
        newSysJobs <- readConfigRepo >>= calculateNextInstances
        modifyIORef queueRef (& systemJobs .~ newSysJobs)

    time  <- getCurrentTime
    queue <- readIORef queueRef
    let (dueUsr,upUsr) = splitJobsDue time (queue^.userJobs)
    let (dueSys,upSys) = splitJobsDue time (queue^.systemJobs)

    failedJobs <- mapM executeJob dueSys <&> catMaybes
    mapM_ executeJob dueUsr

    writeIORef queueRef $ Queue (mergeJobs upSys failedJobs) upUsr
