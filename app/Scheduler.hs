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

module Scheduler (schedule) where

import DatabaseUtil
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
import qualified Database.Persist.MySQL as MySQL (get)
import System.Directory

-- TODO: Implement prettier instance of Show
data JobTemplate = JobTemplate {_scheduleFormat :: Schedule, _repoPath :: String, _playbook :: String, _failCount :: Int, _systemJob :: Bool, _repoIdentifier :: String}
    deriving (Show)
data Job = Job {_timeDue :: LocalTime, _templateName :: String}
    deriving (Eq, Show)

type JobTemplates = M.Map String JobTemplate
type Jobs = [Job]

instance Ord Job where
    compare j1 j2 = compare (_timeDue j1) (_timeDue j2)

makeLenses ''Job
makeLenses ''JobTemplate
makeLensesFor [("localTimeOfDay", "localTimeOfDayL")] ''LocalTime

getTime :: IO LocalTime
getTime = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    return $ utcToLocalTime timezone now

-- |Calculates the next job instances from the templates
calculateNextInstances :: StateT JobTemplates IO Jobs
calculateNextInstances = do
    templates <- get
    time <- liftIO getTime
    return $ map (calculateNextInstance time) $ M.toList templates

calculateNextInstance :: LocalTime -> (String,JobTemplate) -> Job
calculateNextInstance time (name,templ) = Job {_timeDue = nextInstance time (templ^.scheduleFormat), _templateName = name}

getDueJobs :: Jobs -> StateT JobTemplates IO Jobs
getDueJobs jobs = do
    time <- liftIO getTime <&> (& localTimeOfDayL %~ (`addTimeOfDay` TimeOfDay{todHour=0,todMin=1,todSec=0})) -- Rounds to the next second
    return $ takeWhile (\j -> j^.timeDue <= time) (sort jobs)

-- |Executes the jobs and removes the user jobs from the job templates
executeJobs :: ConnectionPool -> Jobs -> StateT JobTemplates IO ()
executeJobs pool jobs = do
    mapM_ (executeJob pool) jobs
    modify $ M.filter (^. systemJob)

executeJob :: ConnectionPool -> Job -> StateT JobTemplates IO ()
executeJob pool job = do
    template <- get
    when (maybe False (\x -> x ^. failCount <= schedulerFailMax) (template ^? ix (job^.templateName))) $ do
        time <- liftIO getTime
        runKey <- liftIO $ addRun (Run 1 (takeWhile (/= '.') (show time))) pool
        success <- liftIO $ execPlaybook pool runKey AnsiblePlaybook{executionPath=template `dot` repoPath, playbookName=template `dot` playbook, executeTags="", targetLimit=""} -- TODO: Add support for tags and limit when Executor has it
        let status = if success then 0 else -1 in liftIO $ runSqlPool (update runKey [RunStatus =. status]) pool
        put $ template & ix (job^.templateName) %~ (& failCount %~ if success then const 0 else (+1))
            where
                dot template f = template^.ix (job^.templateName) . f  -- TODO: This needs GADTs, figure out why

-- Read Project from Database, look if exisits
--   No  -> Write Failed in Project table
--   Yes -> Clone/Update Repo Write Run with Run status running and pass key to exec 
--   Delte folder if clone update fail
--     Parse and fill JobTemplates
--       Failed to parse -> Write Failed run in Databse
updateConfigRepoJobTemplates :: ConnectionPool -> JobTemplates -> IO JobTemplates
updateConfigRepoJobTemplates pool templ = do
    projects <- getProjects pool
    return $ M.fromList [("TestJob1", JobTemplate{_scheduleFormat=scheduleNext, _repoPath="ansible-example", _playbook="pb.yml", _failCount=0, _systemJob=False, _repoIdentifier=""})]  -- TODO: Remove this is for testing

createAndFillFolder :: String -> String -> IO (Either String Bool)
createAndFillFolder url branch = do
    andM [doesDirectoryExist path, (==0) <$> isRepo path url, (==0) <$> doPull url branch]
        >>= \b -> if b then return (Right True)
                       else (\i -> if i==0 then Right True else Left "") <$> (createDirectory path >> doClone path url branch) --TODO Change
        where path = projectUrlToPath url



readJobsDatabase :: ReaderT SqlBackend IO JobTemplates
readJobsDatabase = do
    dataJoin <- joinJobQueuePlaybookProject >>= removeIllegalJobs
    let templs = map (\(_,x,y) -> createUserTemplate (entityVal x) (entityVal y)) dataJoin
    mapM_ (delete . entityKey . fst3) dataJoin
    return $ M.fromList $ zip [schedulerUserTemplateKey ++ show n | n <- [0..]] templs

createUserTemplate :: Playbook -> Project -> JobTemplate
createUserTemplate play proj =
    JobTemplate{_scheduleFormat=scheduleNext, _repoPath=projectUrlToPath $ projectUrl proj, _playbook=playbookPlaybookName play, _failCount=0, _systemJob=False, _repoIdentifier=""} --TODO: Add support for repoIdentifier

-- |Assumes ssh format (e.g. git@git.example.com:test/test-repo.git) and return the path (e.g. test/test-repo)
projectUrlToPath :: String -> FilePath
projectUrlToPath = removeGitSuffix . after '/'
    where
        removeGitSuffix = reverse . after '.' . reverse

getDatabaseJobQueue ::  ReaderT SqlBackend IO [Entity JobQueue]
getDatabaseJobQueue = selectList [] [Asc JobQueueId]

joinJobQueuePlaybookProject :: ReaderT SqlBackend IO [(Entity JobQueue, Entity Playbook, Entity Project)]
joinJobQueuePlaybookProject = do
    jobs <- selectList [] [Asc JobQueueId]
    playbooks <- mapM (getJobPlaybook . jobQueuePlaybookId . entityVal) jobs
    projects  <- mapM (getPlaybookProject . playbookProjectId . entityVal) playbooks
    return $ zip3 jobs playbooks projects

-- |Removes all Jobs whose Project is marked as failed
removeIllegalJobs :: [(Entity JobQueue, Entity Playbook, Entity Project)] -> ReaderT SqlBackend IO [(Entity JobQueue, Entity Playbook, Entity Project)]
removeIllegalJobs list = return $ filter (\(_,_,x) -> null $ projectErrorMessage (entityVal x)) list

getJobPlaybook :: PlaybookId -> ReaderT SqlBackend IO (Entity Playbook)
getJobPlaybook playId = head <$> selectList [PlaybookId ==. playId] []

getPlaybookProject :: ProjectId -> ReaderT SqlBackend IO (Entity Project)
getPlaybookProject proId = head <$> selectList [ProjectId ==. proId] []

-- |Given a list of job templates, updates them (force update by passing empty map as argument), reads the user jobs from the databse and executes all due ones
runJobs :: ConnectionPool -> JobTemplates -> IO JobTemplates
runJobs pool jobTempls = do
    jobTempls' <- M.union <$> updateConfigRepoJobTemplates pool jobTempls <*> runSqlPool readJobsDatabase pool
    snd <$> runStateT (calculateNextInstances >>= getDueJobs >>= executeJobs pool) jobTempls'

schedule :: ConnectionPool -> IO ()
schedule pool = do
    jt <- runJobs pool M.empty
    print "Scheduler done"  -- TODO: Change only for Testing
    return ()

-- |A total version of tail . dropWhile (/= x)
after :: Eq a => a -> [a] -> [a]
after x xs = case dropWhile (/= x) xs of
          []  -> []
          xs' -> tail xs'

andM :: Monad m => [m Bool] -> m Bool
andM []     = return True
andM (b:bs) = b >>= \b -> if not b then return False else andM bs

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
