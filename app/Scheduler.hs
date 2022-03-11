{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Scheduler where

import qualified Executor as E
import ScheduleFormat
import Config
import Data.Time.LocalTime.Compat
import Data.Time.Clock.Compat
import Data.List (sort)
import qualified Data.Map as M
import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Trans

data JobTemplate = JobTemplate {_scheduleFormat :: Schedule, _repoPath :: String, _playbook :: String, _failCount :: Int, _systemJob :: Bool}
data Job = Job {_timeDue :: LocalTime, _templateName :: String}
    deriving (Eq)

type JobTemplates = M.Map String JobTemplate
type Jobs = [Job]

instance Ord Job where
    compare j1 j2 = compare (_timeDue j1) (_timeDue j2)

makeLenses ''Job
makeLenses ''JobTemplate

getTime :: IO LocalTime
getTime = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    return $ utcToLocalTime timezone now

-- |Calculates the next job instances form the templates and removes the user job templates
calculateNextInstances :: StateT JobTemplates IO Jobs
calculateNextInstances = do
    templates <- get
    time <- liftIO getTime
    modify $ M.filter (^. systemJob)
    return $ map (calculateNextInstance time) $ M.toList templates

calculateNextInstance :: LocalTime -> (String,JobTemplate) -> Job
calculateNextInstance time (name,templ) = Job {_timeDue = nextInstance time (templ^.schedule), _templateName = name}

getDueJobs :: Jobs -> StateT JobTemplates IO Jobs
getDueJobs jobs = do
    time <- liftIO getTime
    return $ takeWhile (\j -> j^.timeDue <= time) (sort jobs)

executeJobs :: Jobs -> StateT JobTemplates IO ()
executeJobs = mapM_ executeJob

executeJob :: Job -> StateT JobTemplates IO ()
executeJob job = do
    template <- get
    when (maybe False (\x -> x ^. failCount <= schedulerFailMax) (template ^? ix (job^.templateName))) $ do
        liftIO $ E.exec E.AnsiblePlaybook{E.path=template `dot` repoPath, E.name=template `dot` playbook, E.tags="", E.limit=""} -- TODO: Add support for tags and limit when Executor has it
        let success = False -- TODO: Remove
        put $ template & ix (job^.templateName) %~ (& failCount %~ if success then const 0 else (+1))
            where
                dot template f = template^.ix (job^.templateName) . f  -- TODO: This needs GADTs, figure out why

-- Read Project from Database, look if exisits
--   No  -> Write Failed run in Databse
--   Yes -> Clone/Update Repo
--     Parse and fill JobTemplates
--       Failed to parse -> Write Failed run in Databse
updateConfigRepoJobTemplates :: JobTemplates -> IO JobTemplates
updateConfigRepoJobTemplates = undefined

readJobsDatabase :: IO JobTemplates
readJobsDatabase = undefined

-- |Given a list of initial job templates (the ones from the config repo), updates them, reads the user jobs from the databse and executes all due ones
runJobs :: JobTemplates -> IO JobTemplates
runJobs jobTempls = do
    jobTempls' <- mappend <$> updateConfigRepoJobTemplates jobTempls <*> readJobsDatabase    -- somehow (++) doesn't work, therefore I used mappend
    snd <$> runStateT (calculateNextInstances >>= getDueJobs >>= executeJobs) jobTempls'

schedule = undefined
