{- app/Scheduler.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Scheduler where

import Main
import Git
import Executor
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
import Control.Monad.Trans.Reader
import Database.Persist.MySQL hiding (get)

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
calculateNextInstance time (name,templ) = Job {_timeDue = nextInstance time (templ^.scheduleFormat), _templateName = name}

getDueJobs :: Jobs -> StateT JobTemplates IO Jobs
getDueJobs jobs = do
    time <- liftIO getTime
    return $ takeWhile (\j -> j^.timeDue <= time) (sort jobs)

executeJobs :: ConnectionPool -> Jobs -> StateT JobTemplates IO ()
executeJobs cp = mapM_ (executeJob cp)

executeJob :: ConnectionPool -> Job -> StateT JobTemplates IO ()
executeJob cp job = do
    template <- get
    when (maybe False (\x -> x ^. failCount <= schedulerFailMax) (template ^? ix (job^.templateName))) $ do
        success <- liftIO $ execPlaybook cp AnsiblePlaybook{executionPath=template `dot` repoPath, playbookName=template `dot` playbook, executeTags="", targetLimit=""} -- TODO: Add support for tags and limit when Executor has it
        put $ template & ix (job^.templateName) %~ (& failCount %~ if success then const 0 else (+1))
            where
                dot template f = template^.ix (job^.templateName) . f  -- TODO: This needs GADTs, figure out why

-- Read Project from Database, look if exisits
--   No  -> Write Failed in Project table
--   Yes -> Clone/Update Repo Write Run with Run status running and pass key to exec 
--   Delte folder if clone update fail
--     Parse and fill JobTemplates
--       Failed to parse -> Write Failed run in Databse
updateConfigRepoJobTemplates :: JobTemplates -> IO JobTemplates
updateConfigRepoJobTemplates _ = do
    return $ M.fromList [("asd", JobTemplate{_scheduleFormat=now, _repoPath="ansible-example", _playbook="pb.yml", _failCount=0, _systemJob=False})]  -- TODO: Remove this is for testing
        where now=  Schedule{_scheduleDay=fullWeek, _scheduleTime=Just ScheduleTime{_startTime=allFullHours, _repetitionTime=[TimeOfDay{todHour=0,todMin=1,todSec=0}]}}

-- TODO: Playbook mithilfe des foreign key auslesen und daraus Job erstellen
-- TODO: Falls Project fuer Job als failed markiert ist, kein JobTemplate erstellen und auch ?nicht aus der Datenbank loeschen
readJobsDatabase :: ReaderT SqlBackend IO JobTemplates
readJobsDatabase = do
    --jobsRaw <- getDatabaseJobQueue
    --removeDatabseJobQueue $ map entityKey jobsRaw
    return M.empty

getJobPlaybook :: PlaybookId -> ReaderT SqlBackend IO (Entity Playbook)
getJobPlaybook playId = do
    --let a = getBy playId :: ReaderT SqlBackend IO (Maybe (Entity Playbook))
    head <$> selectList [PlaybookId ==. playId] [] --TODO: Is this list never empty?

getDatabaseJobQueue ::  ReaderT SqlBackend IO [Entity JobQueue]
getDatabaseJobQueue = selectList [] [Asc JobQueueId]

removeDatabseJobQueue :: [Key JobQueue] -> ReaderT SqlBackend IO ()
removeDatabseJobQueue = mapM_ delete

-- |Given a list of job templates, updates them (force update by passing empty map as argument), reads the user jobs from the databse and executes all due ones
runJobs :: ConnectionPool -> JobTemplates -> IO JobTemplates
runJobs pool jobTempls = do
    jobTempls' <- mappend <$> updateConfigRepoJobTemplates jobTempls <*> runSqlPool readJobsDatabase pool
    snd <$> runStateT (calculateNextInstances >>= getDueJobs >>= executeJobs pool) jobTempls'

schedule :: ConnectionPool -> IO ()
schedule pool = do
    jt <- runJobs pool M.empty
    return ()
