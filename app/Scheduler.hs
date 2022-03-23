{- app/Scheduler.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Scheduler (schedule) where

import DatabaseUtil
import ProjectConfig
import Git
import Executor
import ScheduleFormat
import Config

import Data.Char (isAlphaNum)
import Data.Time.LocalTime.Compat (LocalTime, TimeOfDay (..), getCurrentTimeZone, utcToLocalTime)
import Data.Time.Clock.Compat (getCurrentTime)
import Data.List ((\\), foldl')
import Data.Text (pack, Text, intercalate)
import qualified Data.Map as M (Map, empty, filter, toList, union, unions, delete, fromList, singleton)
import Control.Lens((^.),(%~),(%=),(<&>),(&),(.~),(.=),_1,_Right,view,at,mapped,makeLenses,makeLensesFor)
import Control.Lens.Combinators (filtered)
import Control.Monad (when, filterM, unless )
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.RWS (RWST, execRWST, ask, liftIO, lift, get, gets, modify)
import Control.Concurrent (threadDelay)
import Control.Monad.Logger (MonadLogger, LoggingT, runStderrLoggingT, logInfoNS, logDebugNS)
import Database.Persist.MySQL hiding (get)
import System.Directory (listDirectory, removeDirectoryRecursive, doesDirectoryExist)

-- TODO: Implement prettier instance of Show
data Job = Job {_timeDue :: LocalTime, _scheduleFormat :: Schedule, _repoPath :: String, _playbook :: String, _playbookId :: PlaybookId, _failCount :: Int, _systemJob :: Bool, _repoIdentifier :: String}
-- The key is the project path (git project name plus databse id) plus the playbook
type Jobs = M.Map String Job
-- TODO: We probably don't want to use monad logger, and instead use the writer in the RWST stack
type JobEnv = RWST ConnectionPool () Jobs (LoggingT IO)

makeLenses ''Job

instance Eq Job where
    j1 == j2 = j1^.timeDue == j2^.timeDue

instance Ord Job where
    compare j1 j2 = compare (j1^.timeDue) (j2^.timeDue)

showJobsT :: Jobs -> Text
showJobsT = intercalate ", " . map (\(k,v) -> showT k <> " at " <> showJob v) . M.toList
    where showJob j = pack $ takeWhile (/= '.') $ show (j^.timeDue)

schedule :: ConnectionPool -> IO ()
schedule pool = runStderrLoggingT $ runJobs pool M.empty

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
    jobs <- gets $ M.filter ((>=) time . view timeDue)
    traverse . filtered ((>=) time . view timeDue) %= calAndInsertNextInstance time
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
    logInfo $ "Schedule job: `" <> showJobsT (M.singleton name job)
    success <- fmap (ExecutorNoErr ==) $ lift $
        execPlaybook pool runKey AnsiblePlaybook{executionPath=job^.repoPath, playbookName=job^.playbook, executeTags="", targetLimit=""} -- TODO: Add support for tags and limit when Executor has it
    logInfo $ "Job '" <> pack name <> "' finished with status " <> showT success
    let status = if success then 0 else -1 in lift' $ update runKey [RunStatus =. status]
    at name %= ((mapped.failCount) %~ if success then const 0 else (+1))


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
    mapM_ updateSystemJobs projects
    get >>= logDebug . mappend "System jobs: ". showJobsT

-- | When a project is deleted in the database remove the folder and it's jobs
cleanupFoldersAndJobs :: [Entity Project] -> JobEnv ()
cleanupFoldersAndJobs projects = do
    existingFolders <- liftIO $ listDirectory schedulerRepoRoot >>= filterM doesDirectoryExist
    let remove = existingFolders \\ (map projectToPath projects ++ schedulerFolders)
    liftIO $ mapM_ removeDirectoryRecursive remove
    unless (null remove) $ logDebug ("I removed these folders: " <> showT remove)
    modify $ \j -> foldl' (flip M.delete) j remove

-- | Updates the system jobs if the repo changed or doesn't locally exist, else leaves them unchanged
updateSystemJobs :: Entity Project -> JobEnv ()
updateSystemJobs project = do
    let oid = projectOid $ entityVal project
    jobsNull <- gets null
    ret <- liftIO $ getRepo oid path url branch <&> (& (_Right._1.filtered (const jobsNull)) .~ True)
    case ret of
      Left  e            -> markProjectFailed (entityKey project) e
      Right (change,oid) -> do
        lift' $ update (entityKey project) [ProjectErrorMessage =. "", ProjectOid =. oid]
        when change $ do
            readAndParseConfigFile oid path project
    where
        url = projectUrl $ entityVal project
        path = projectToPath project
        branch = projectBranch $ entityVal project

getJobsFromProject :: Jobs -> Entity Project -> Jobs
getJobsFromProject jobs project = M.filter (\t -> t^.repoPath == path) jobs
    where path = projectToPath project

-- | Tries to parse the config file in the folder pointed to by path.
-- Writes the parse status and the playbooks specified in the config file in the database and creates the jobs
readAndParseConfigFile :: String -> FilePath -> Entity Project -> JobEnv ()
readAndParseConfigFile oid path p = do
    pcs <- liftIO $ parseConfigFile path
    time <- liftIO getTime
    if null pcs then lift' $ update (entityKey p) [ProjectErrorMessage =. "Error in the config file"]
                else do
                    keys <- mapM (writePlaybookInDatabase (entityKey p)) pcs
                    mapM_ (\(key, job) -> at key .= Just job) $
                        zipWith (curry (createJobsFromPlaybookConfiguration time oid path)) keys pcs

-- | Given an oid, path, a playbook kay and the parsed config, created a job
createJobsFromPlaybookConfiguration :: LocalTime -> String -> String -> (Key Playbook, PlaybookConfiguration) -> (String,Job)
createJobsFromPlaybookConfiguration time oid path (key, p) = (path ++ pName p,
    Job{_timeDue=nextInstance time (pSchedule p), _scheduleFormat=pSchedule p, _repoPath=path, _playbook=pFile p, _playbookId=key, _failCount=0, _systemJob=True, _repoIdentifier=oid})

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
          return $ Right $ if oidNew == oid then (False, oid)
                                            else (True, oidNew)

-- | Recrates the folder, by removing it (if it existed) and cloning the repo
fillFolder :: String -> String -> String -> IO (Either String (Bool,String))
fillFolder path url branch = liftIO (removeDicIfExists path) >> doClone url path branch >>= getOidAfterAction
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
    modify $ M.union $ M.fromList $ zip [show n ++ schedulerUserTemplateKey | n <- [0..]] templs

createUserJobs :: LocalTime -> Entity Playbook -> Entity Project -> Job
createUserJobs time play proj =
    Job{_timeDue=time, _scheduleFormat=Now, _repoPath=projectToPath proj, _playbook=playbookFile $ entityVal play,
                _playbookId=entityKey play, _failCount=0, _systemJob=False, _repoIdentifier=projectOid (entityVal proj)}

-- | Assumes ssh format (e.g. @git@git.example.com:test/test-repo.git@) and return the path (e.g. @test/test-repo@)
-- or an empty string on failure
projectToPath :: Entity Project -> FilePath
projectToPath p = let path = removeGitSuffix (after '/' (projectUrl (entityVal p))) ++ show (fromSqlKey (entityKey p))
                  in if validRepoName path then path else ""
    where
        removeGitSuffix = reverse . after '.' . reverse

-- | Tests if a string is a valid repo name. A repository name should contain only alphanumeric,
-- dash (@'-'@), underscore (@'_'@) and dot (@'.'@) characters, but should not be euqal to @..@.
validRepoName :: String -> Bool
validRepoName ".." = False
validRepoName s    = all chech s
    where chech c = isAlphaNum c || c `elem` ['-','_','.']

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

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t e = b >>= \b -> if b then t else e

-- | A total version of @tail . dropWhile (/= x)@
after :: Eq a => a -> [a] -> [a]
after x xs = case dropWhile (/= x) xs of
          []  -> []
          xs' -> tail xs'

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

logInfo :: MonadLogger m => Text -> m ()
logInfo = logInfoNS "Scheduler"

logDebug :: MonadLogger m => Text -> m ()
logDebug = logDebugNS "Scheduler"

showT :: Show a => a -> Text
showT = pack . show

lift' f = do
    pool <- ask
    liftIO $ runSqlPool f pool

-- \MIC\ --
