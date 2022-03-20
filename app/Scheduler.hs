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

--module Scheduler (schedule) where
module Scheduler where

import DatabaseUtil
import ProjectConfig
import Git
import Executor
import ScheduleFormat
import Config

import Data.Char (isAlphaNum)
import Data.Time.LocalTime.Compat (LocalTime, TimeOfDay (..), getCurrentTimeZone, utcToLocalTime, addLocalTime)
import Data.Time.Clock.Compat (getCurrentTime)
import Data.List (sort, (\\), foldl', nub)
import Data.Maybe (isJust)
import qualified Data.Map as M (Map, empty, filter, toList, union, unions, delete, fromList, singleton, keys, adjust, map, update)
import Control.Lens ((^.), (%~), (%=), (<&>), (&), (?~), at, use, mapped, makeLenses, makeLensesFor)
import Control.Monad (when)
import Control.Monad.State (StateT, gets, runStateT, liftIO, modify, filterM, get, put)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Concurrent (threadDelay)
import Database.Persist.MySQL hiding (get)
import System.Directory (listDirectory, removeDirectoryRecursive, doesDirectoryExist)

-- TODO: Implement prettier instance of Show
data Job = Job {_timeDue :: Maybe LocalTime, _scheduleFormat :: Schedule, _repoPath :: String, _playbook :: String, _playbookId :: PlaybookId, _failCount :: Int, _systemJob :: Bool, _repoIdentifier :: String}
type Jobs = M.Map String Job

makeLenses ''Job

instance Show Job where
    show j = j^.playbook ++ " at " ++ show (j^.timeDue)

instance Eq Job where
    j1 == j2 = j1^.timeDue == j2^.timeDue

instance Ord Job where
    compare j1 j2 = compare (j1^.timeDue) (j2^.timeDue)


schedule :: ConnectionPool -> IO ()
schedule pool = runJobs pool M.empty

-- | Given a list of job templates, updates them (force update by passing empty map as argument), reads the user jobs from the databse and executes all due ones
runJobs :: ConnectionPool -> Jobs -> IO ()
runJobs pool jobs = do
    jobs' <- runSqlPool (updateConfigRepoJobs jobs >>= (\u -> M.union u <$> readJobQueueDatabase)) pool --Have different timing for the two updates
    js <- snd <$> runStateT (getDueJobs >>= executeJobs pool) jobs'
    threadDelay 10000000
    runJobs pool js

-- TODO: Cleaner and Docu
getDueJobs :: StateT Jobs IO Jobs
getDueJobs = do
    time <- liftIO getTime
    jobs <- get
    let jobsDue = M.filter (\j -> maybe True (<= time) (j^.timeDue)) jobs
    let jobsDue' = foldl (flip $ M.update (\j -> Just $ j & timeDue ?~ nextInstance time (j^.scheduleFormat))) jobs (M.keys jobsDue)
    put jobsDue'
    return $ M.filter (\j -> maybe False (<= time) (j^.timeDue)) jobsDue'

-- | Executes the jobs and removes the user jobs from the job templates
executeJobs :: ConnectionPool -> Jobs -> StateT Jobs IO ()
executeJobs pool jobs = do
    mapM_ (executeJob pool) $ M.toList jobs
    modify $ M.filter (^. systemJob)

-- | Executes a job and writes the run into the database
-- If the job failes, increases the failcount, else resets it
executeJob :: ConnectionPool -> (String,Job) -> StateT Jobs IO ()
executeJob pool (name,job) = do
      time <- liftIO getTime
      runKey <- liftIO $ addRun (Run (job^.playbookId)
                                     1
                                     (job^.repoIdentifier)
                                     (job^.systemJob)
                                     (takeWhile (/= '.') (show time))) pool
      success <- liftIO $ execPlaybook pool runKey AnsiblePlaybook{executionPath=job^.repoPath, playbookName=job^.playbook, executeTags="", targetLimit=""} -- TODO: Add support for tags and limit when Executor has it
      let status = if success then 0 else -1 in liftIO $ runSqlPool (update runKey [RunStatus =. status]) pool
      at name %= ((mapped.failCount) %~ if success then const 0 else (+1))


-- /SYSTEM JOBS/ --

-- Read project from the datatbase
-- Cleanup of folders and templates if a project is removed from the database
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
-- Create templates
updateConfigRepoJobs :: Jobs -> ReaderT SqlBackend IO Jobs
updateConfigRepoJobs jobs = do
    projects <- getProjects
    liftIO $ cleanupFoldersAndJobs jobs projects
    mapM (updateSystemJobs jobs) projects <&> M.unions

-- | When a project is deleted in the database remove the folder and it's job templates
cleanupFoldersAndJobs :: Jobs -> [Entity Project] -> IO Jobs
cleanupFoldersAndJobs jobs projects = do
    existingFolders <- listDirectory schedulerRepoRoot >>= filterM doesDirectoryExist
    let remove = existingFolders \\ (map projectToPath projects ++ schedulerFolders)
    liftIO $ print $ "INFO: Scheduler removed these folders: " ++ show remove -- TODO: Replace with logging
    mapM_ removeDirectoryRecursive remove
    return $ foldl' (flip M.delete) jobs remove

-- | Updates the system templates if the repo changed or doesn't locally exist, else leaves them unchanged
updateSystemJobs :: Jobs -> Entity Project -> ReaderT SqlBackend IO Jobs
updateSystemJobs jobs project = do
    let oid = projectOid $ entityVal project
    ret' <- liftIO $ getRepo oid path url branch
    let ret = ret' <&> if null jobs then \(_,x) -> (True, x) else id
    case ret of
      Left  e            -> markProjectFailed (entityKey project) e >> return M.empty
      Right (change,oid) -> do
        update (entityKey project) [ProjectErrorMessage =. "", ProjectOid =. oid]
        if change then readAndParseConfigFile oid path project
                  else return $ getJobsFromProject jobs project
    where
        url = projectUrl $ entityVal project
        path = projectToPath project
        branch = projectBranch $ entityVal project

getJobsFromProject :: Jobs -> Entity Project -> Jobs
getJobsFromProject jobs project = M.filter (\t -> t^.repoPath == path) jobs
    where path = projectToPath project

-- | Tries to parse the config file in the folder pointed to by path.
-- Writes the parse status and the playbooks specified in the config file in the database and creates the job-tomplates
readAndParseConfigFile :: String -> FilePath -> Entity Project -> ReaderT SqlBackend IO Jobs
readAndParseConfigFile oid path p = do
    pcs <- liftIO $ parseConfigFile path
    if null pcs then update (entityKey p) [ProjectErrorMessage =. "Error in the config file"] >> return M.empty else do
        keys <- mapM (writePlaybookInDatabase (entityKey p)) pcs
        return $ M.unions $ zipWith (createJobsFromPlaybookConfiguration oid path) keys pcs

-- | Given an oid, path, a playbook kay and the parsed config, created an job-template
createJobsFromPlaybookConfiguration :: String -> String -> Key Playbook -> PlaybookConfiguration -> Jobs
createJobsFromPlaybookConfiguration oid path key p = M.singleton (pName p)
    Job{_timeDue=Nothing, _scheduleFormat=pSchedule p, _repoPath=path, _playbook=pFile p, _playbookId=key, _failCount=0, _systemJob=True, _repoIdentifier=oid}

markProjectFailed :: Key Project -> String -> ReaderT SqlBackend IO ()
markProjectFailed key e = update key [ProjectErrorMessage =. e]

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

readJobQueueDatabase :: ReaderT SqlBackend IO Jobs
readJobQueueDatabase = do
    dataJoin <- joinJobQueuePlaybookProject >>= removeIllegalJobs
    let templs = map (\(_,x,y) -> createUserJobs x y) dataJoin
    mapM_ (delete . entityKey . fst3) dataJoin
    return $ M.fromList $ zip [schedulerUserTemplateKey ++ show n | n <- [0..]] templs

createUserJobs :: Entity Playbook -> Entity Project -> Job
createUserJobs play proj =
    Job{_timeDue=Nothing, _scheduleFormat=Now, _repoPath=projectToPath proj, _playbook=playbookFile $ entityVal play,
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
joinJobQueuePlaybookProject :: ReaderT SqlBackend IO [(Entity JobQueue, Entity Playbook, Entity Project)]
joinJobQueuePlaybookProject = do
    jobs <- selectList [] [Asc JobQueueId]
    playbooks <- mapM (getJobPlaybook . jobQueuePlaybookId . entityVal) jobs
    projects  <- mapM (getPlaybookProject . playbookProjectId . entityVal) playbooks
    return $ zip3 jobs playbooks projects

-- | Removes all jobs whose project are marked as failed
removeIllegalJobs :: [(Entity JobQueue, Entity Playbook, Entity Project)] -> ReaderT SqlBackend IO [(Entity JobQueue, Entity Playbook, Entity Project)]
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

-- \MIC\ --
