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
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Scheduler (schedule) where

import DatabaseUtil
import ProjectConfig
import Git
import Executor
import ScheduleFormat
import Config
import Extra (ifM, fst3, showT)

import Data.Char (isAlphaNum)
import Data.Time.LocalTime.Compat (LocalTime, TimeOfDay (..), getCurrentTimeZone, utcToLocalTime)
import Data.Time.Clock.Compat (getCurrentTime)
import Data.List ((\\), foldl', isPrefixOf)
import Data.Text (pack, Text, intercalate)
import qualified Data.Map as M (Map, empty, filter, toList, union, unions, delete, fromList, singleton)
import Control.Lens
    ( (^.)
    , (%~)
    , (%=)
    , (<&>)
    , (&)
    , (.~)
    , (.=)
    , (^..)
    , (%%@=)
    , _1
    , _Right
    , view
    , at
    , mapped
    , makeLenses
    , makeLensesFor
    , itoList)
import Control.Lens.Combinators (filtered, itraverse, Indexed (runIndexed))
import Control.Monad (when, filterM, unless)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.RWS (RWST, execRWST, ask, liftIO, lift, get, gets, modify)
import Control.Concurrent (threadDelay)
import Control.Monad.Logger (MonadLogger, LoggingT, runStderrLoggingT, logInfoNS, logDebugNS)
import Database.Persist.MySQL
    ( ConnectionPool
    , Entity (entityVal, entityKey)
    , SqlBackend
    , PersistStoreWrite (insert, update, delete)
    , (=.)
    , selectList
    , fromSqlKey
    , SelectOpt (Asc)
    , runSqlPool)
import System.Directory (listDirectory, removeDirectoryRecursive, doesDirectoryExist)

data Job = Job { _timeDue :: LocalTime
               , _scheduleFormat :: Schedule
               , _repoPath :: String
               , _playbook :: String
               , _playbookId :: PlaybookId
               , _failCount :: Int
               , _systemJob :: Bool
               , _repoIdentifier :: String}
-- The key is the project path (git project name plus databse id) plus the playbook
type Jobs = M.Map String Job
type JobEnv = RWST ConnectionPool () Jobs (LoggingT IO)

makeLenses ''Job

instance Eq Job where
    j1 == j2 = j1^.timeDue == j2^.timeDue

instance Ord Job where
    compare j1 j2 = compare (j1^.timeDue) (j2^.timeDue)

showJobsT :: Jobs -> Text
showJobsT = intercalate ", " . map (\(k,v) -> showT k <> " at " <> showJob v) . M.toList
    where showJob j = pack $ takeWhile (/= '.') $ show (j^.timeDue)

schedule :: ConnectionPool -> LoggingT IO ()
schedule pool = runJobs pool M.empty

-- | Given a list of jobs, updates them (force update by passing empty map as argument), reads the user jobs from the databse and executes all due ones
runJobs :: ConnectionPool -> Jobs -> LoggingT IO ()
runJobs pool jobMap = do
    jobs <- fst <$> execRWST (updateConfigRepoJobs >> readJobQueueDatabase >> getDueJobsCalculateTimestamp >>= executeJobs) pool jobMap
    logDebug $ "All Jobs: " <> showJobsT jobs
    lift $ threadDelay 10000000
    runJobs pool jobs

-- | Returns the jobs which are due and calculates the next timestamps
getDueJobsCalculateTimestamp :: JobEnv Jobs
getDueJobsCalculateTimestamp = do
    time <- liftIO getTime
    jobs <- itraverse . runIndexed . filtered ((>=) time . view timeDue) %%@= \i j -> (M.singleton i j, calAndInsertNextInstance time j)
    logDebug $ "Due Jobs: " <> showJobsT jobs
    return jobs
        where calAndInsertNextInstance time j = j & timeDue .~ nextInstance time (j^.scheduleFormat)

-- | Executes the jobs and removes the user jobs from the job templates
executeJobs :: Jobs -> JobEnv ()
executeJobs jobs = do
    mapM_ executeJob $ M.toList jobs
    modify $ M.filter (^. systemJob)

-- | Executes a job and writes the run into the database
-- If the job failed increases the failcount, else resets it
executeJob :: (String,Job) -> JobEnv ()
executeJob (name,job) = do
    time <- liftIO getTime
    pool <- ask
    runKey <- lift' $ insert $ Run (job^.playbookId)
                                    1
                                   (job^.repoIdentifier)
                                   (job^.systemJob)
                                   (takeWhile (/= '.') (show time))
    logInfo $ "Schedule job: " <> showJobsT (M.singleton name job)
    success <- lift $ execPlaybook pool runKey
                -- TODO: Add support for tags and limit when Executor has it
                AnsiblePlaybook { executionPath=job^.repoPath
                                , playbookName=job^.playbook
                                , executeTags=""
                                , targetLimit=""}
    logInfo $ "Job '" <> pack name <> "' finished with status " <> showT success
    let (status, modifyFail) = if success == ExecutorNoErr then (0, const 0) else (-1, (+1))
        in lift' (update runKey [RunStatus =. status])
        >> at name %= (mapped.failCount %~ modifyFail)


-- /SYSTEM JOBS/ --

-- Read project from the datatbase
-- Cleanup of folders and jobs if a project is removed from the database
-- For every project:
--   Check if it locally exists:
--      No  -> Clone
--      Yes -> Check if it is a valid repo
--         No  -> Delete Folder
--               Clone
--         Yes -> Check if something changed:
--             Yes -> Pull
--             No  -> do nothing
-- If something happend above, parse config file
-- Create jobs
updateConfigRepoJobs :: JobEnv ()
updateConfigRepoJobs = do
    projects <- lift' getProjects
    cleanupFoldersAndJobs projects
    gets null >>= \force -> mapM_ (updateSystemJobs force) projects
    get >>= logDebug . mappend "System jobs: " . showJobsT

-- | When a project is deleted in the database remove the folder and its jobs
cleanupFoldersAndJobs :: [Entity Project] -> JobEnv ()
cleanupFoldersAndJobs projects = do
    existingFolders <- liftIO $ listDirectory schedulerRepoRoot >>= filterM doesDirectoryExist
    let remove = existingFolders \\ (map projectToPath projects ++ schedulerFolders)
    unless (null remove) $ do
        liftIO $ mapM_ removeDirectoryRecursive remove
        logDebug ("I removed these folders: " <> showT remove)
        -- TODO: This should be possible without converting the map to a list (ifiltered?)
        modify $ M.fromList . \jobs -> itoList jobs^..traverse.filtered (\(key,_) -> old key remove)
                where old key paths = any (`isPrefixOf` key) paths

-- | Updates the system jobs if the repo changed or doesn't locally exist, else leaves them unchanged
updateSystemJobs :: Bool -> Entity Project -> JobEnv ()
updateSystemJobs force project = do
    let oid = projectOid $ entityVal project
    ret <- liftIO $ getRepo oid path url branch <&> (& _Right._1.filtered (const force) .~ True)
    case ret of
      Left  e            -> markProjectFailed (entityKey project) e
      Right (change,oid) -> do
        lift' $ update (entityKey project) [ProjectErrorMessage =. "", ProjectOid =. oid]
        when change $
            logInfo ("The repo " <> showT url <> " changed, so I updated it's jobs")
                >> readAndParseConfigFile oid path project
    where
        url = projectUrl $ entityVal project
        path = projectToPath project
        branch = projectBranch $ entityVal project

-- | Tries to parse the config file in the folder pointed to by path.
-- Writes the parse status and the playbooks specified in the config file in the database and creates/updates the jobs
readAndParseConfigFile :: String -> FilePath -> Entity Project -> JobEnv ()
readAndParseConfigFile oid path p = do
    pcs <- liftIO $ parseConfigFile path
    time <- liftIO getTime
    case pcs of
      Left  e -> lift' $ update (entityKey p) [ProjectErrorMessage =. e]
      Right v -> do
          keys <- mapM (writePlaybookInDatabase (entityKey p)) v
          mapM_ (\(key, job) -> at key .= Just job) $
              zipWith (curry (createJobsFromPlaybookConfiguration time oid path)) keys v

-- | Given an oid, path, a playbook kay and the parsed config, created a job
createJobsFromPlaybookConfiguration :: LocalTime -> String -> String -> (Key Playbook, PlaybookConfiguration) -> (String,Job)
createJobsFromPlaybookConfiguration time oid path (key, p) =
    (path ++ '-' : pName p, Job { _timeDue=nextInstance time (pSchedule p)
                                , _scheduleFormat=pSchedule p
                                , _repoPath=path
                                , _playbook=pFile p
                                , _playbookId=key
                                , _failCount=0
                                , _systemJob=True
                                , _repoIdentifier=oid})

markProjectFailed :: Key Project -> String -> JobEnv ()
markProjectFailed key e = lift' $ update key [ProjectErrorMessage =. e]

-- | Creates or updates the repo folder if necessary
getRepo :: String -> String -> String -> String -> IO (Either String (Bool,String))
getRepo oid path url branch =
    ifM (liftIO (doesDirectoryExist path)) (updateFolder oid path url branch) (fillFolder path url branch)

-- | Updates the repo folder if necessary
updateFolder :: String -> String -> String -> String -> IO (Either String (Bool,String))
updateFolder oid path url branch =
    ifM (liftIO (isRepo path url))
        (doPullPerhaps oid path branch)
        (fillFolder path url branch)

-- | Pulls if there is an update
doPullPerhaps :: String -> String -> String -> IO (Either String (Bool,String))
doPullPerhaps oid path branch = do
    pull <- doPull path branch
    case pull of
      Left err -> return $ Left err
      Right () -> do
          oidNew <- getLastOid
          return $ Right (oid /= oidNew, oidNew)

-- | Recrates the folder, by removing it (if it existed) and cloning the repo
fillFolder :: String -> String -> String -> IO (Either String (Bool,String))
fillFolder path url branch = removeDicIfExists path >> doClone url path branch >>= getOidAfterAction
    where removeDicIfExists path = doesDirectoryExist path >>= \b -> when b $ removeDirectoryRecursive path

-- | Returns the oid after a pull or clone while respecting failures
getOidAfterAction :: Either String () -> IO (Either String (Bool,String))
getOidAfterAction retAction = getLastOid >>= \oid -> return $ (True,oid) <$ retAction

-- \SYSTEM JOB\ --


-- /JOB QUEUE/ --

readJobQueueDatabase :: JobEnv ()
readJobQueueDatabase = do
    dataJoin <- joinJobQueuePlaybookProject >>= removeIllegalJobs
    time <- liftIO getTime
    let templs = map (\(_,x,y) -> createUserJobs time x y) dataJoin
    mapM_ (lift' . delete . entityKey . fst3) dataJoin
    modify $ M.union $ M.fromList $ zip [schedulerUserTemplateKey ++ "-" ++ show n | n <- [0..]] templs

createUserJobs :: LocalTime -> Entity Playbook -> Entity Project -> Job
createUserJobs time play proj =
    Job { _timeDue=time
        , _scheduleFormat=Now
        , _repoPath=projectToPath proj
        , _playbook=playbookFile $ entityVal play
        , _playbookId=entityKey play
        , _failCount=0
        , _systemJob=False
        , _repoIdentifier=projectOid (entityVal proj)}

-- | Assumes ssh format (e.g. @git@git.example.com:test/test-repo.git@) and return the path (e.g. @test/test-repo@)
-- or an empty string on failure
projectToPath :: Entity Project -> FilePath
projectToPath p = "REPO" ++ show (fromSqlKey (entityKey p))

getDatabaseJobQueue :: ReaderT SqlBackend IO [Entity JobQueue]
getDatabaseJobQueue = selectList [] [Asc JobQueueId]

-- | Joins the job-queue, playbooks and projects
joinJobQueuePlaybookProject :: JobEnv [(Entity JobQueue, Entity Playbook, Entity Project)]
joinJobQueuePlaybookProject = do
    jobs <- lift' (selectList [] [Asc JobQueueId])
    playbooks <- mapM (lift' . getJobPlaybook . jobQueuePlaybookId . entityVal) jobs
    projects  <- mapM (lift' . getPlaybookProject . playbookProjectId . entityVal) playbooks
    return $ zip3 jobs playbooks projects

-- | Removes all jobs whose project are marked as failed
removeIllegalJobs :: [(Entity JobQueue, Entity Playbook, Entity Project)] -> JobEnv [(Entity JobQueue, Entity Playbook, Entity Project)]
removeIllegalJobs list = return $ filter (\(_,_,x) -> null $ projectErrorMessage (entityVal x)) list

-- \JOB QUEUE\ --


-- /MISC/ --

getTime :: IO LocalTime
getTime = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    return $ utcToLocalTime timezone now

logInfo :: MonadLogger m => Text -> m ()
logInfo = logInfoNS "Scheduler"

logDebug :: MonadLogger m => Text -> m ()
logDebug = logDebugNS "Scheduler"

lift' action = do
    pool <- ask
    liftIO $ runSqlPool action pool

-- \MIC\ --
