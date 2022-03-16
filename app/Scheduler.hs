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

module Scheduler (schedule) where

import DatabaseUtil
import Git
import Executor
import ScheduleFormat
import Config
import Data.Time.LocalTime.Compat
import Data.Time.Clock.Compat
import Data.List (sort, (\\), foldl')
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
data JobTemplate = JobTemplate {_scheduleFormat :: Schedule, _repoPath :: String, _playbook :: String, _playbookId :: PlaybookId, _failCount :: Int, _systemJob :: Bool, _repoIdentifier :: String}
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
    templates <- get
    case templates ^? ix (job^.templateName) of
      Nothing       -> error "Internal error, a job was created from a nonexistent job template."
      Just template -> when (template^.failCount <= schedulerFailMax) $ do
          time <- liftIO getTime
          runKey <- liftIO $ addRun (Run (template^.playbookId) 1 (template^.repoIdentifier) (takeWhile (/= '.') (show time))) pool
          success <- liftIO $ execPlaybook pool runKey AnsiblePlaybook{executionPath=template^.repoPath, playbookName=template^.playbook, executeTags="", targetLimit=""} -- TODO: Add support for tags and limit when Executor has it
          let status = if success then 0 else -1 in liftIO $ runSqlPool (update runKey [RunStatus =. status]) pool
          put $ templates & ix (job^.templateName) %~ (& failCount %~ if success then const 0 else ( +1))

-- Projecte aus der Datenbank lesen
-- Für jedes Projekt:
--   Schauen ob der Ordner schon existiert:
--     Nein -> Clone ausführen und templates entfernen (f/s)
--     Ja -> Schauen ob das eine Repo ist
--       Nein -> Ordner löschen
--               Clone ausführen und templates update (f/s)
--       Ja   -> Schaun ob sich die oid geändert hat
--          Ja -> Pull ausführen und templates entfernen (f/s)
--          Nein -> nichts tun
--  Nun Config Datei parsen (f/s) (falls ordner erneuert)
--  Daraus Templates erstellen
--  TODO: Wenn programm neugestaretet wird (leere JobTemplates), einmal alle Templates neu erstellen
updateConfigRepoJobTemplates :: JobTemplates -> ReaderT SqlBackend IO JobTemplates
updateConfigRepoJobTemplates templs = do
    projects <- getProjects
    liftIO $ cleanupFoldersAndTemplates templs projects
    mapM (updateSystemTemplate templs) projects <&> M.unions

-- |When a project is deleted in the database remove the folder and it's job templates
cleanupFoldersAndTemplates :: JobTemplates -> [Entity Project] -> IO JobTemplates
cleanupFoldersAndTemplates templs projects = do
    existingFolders <- listDirectory "." >>= filterM doesDirectoryExist -- TODO: Will this path stay?
    let remove = existingFolders \\ (map projectToPath projects ++ schedulerFolders)
    mapM_ removeDirectoryRecursive remove
    return $ foldl' (flip M.delete) templs remove

-- |Updates the system templates if the repo changed or doesn't locally exist, else leaves them unchanged
updateSystemTemplate :: JobTemplates -> Entity Project -> ReaderT SqlBackend IO JobTemplates
updateSystemTemplate templs project = do
    let oid = projectOid $ entityVal project
    ret <- liftIO $ getRepo oid path url branch
    case ret of
      Left  e            -> markProjectFailed (entityKey project) e >> return M.empty
      Right (change,oid) -> if change then update (entityKey project) [ProjectErrorMessage =. "", ProjectOid =. oid] >> readAndParseConfigFile oid path project -- TODO: Perhaps put update in the readAndParseConfigFile function
                                      else return $ getTemplatesFromProject templs project
    where
        url = projectUrl $ entityVal project
        path = projectToPath project
        branch = projectBranch $ entityVal project

getTemplatesFromProject :: JobTemplates -> Entity Project -> JobTemplates
getTemplatesFromProject templs project = M.filter (\t -> t^.repoPath == path) templs
    where path = projectToPath project

-- TODO: The parser has to fill the playbook table. Furthermore, if the parsing fails, returns empty Map and marks project as failed
readAndParseConfigFile :: String -> FilePath -> Entity Project -> ReaderT SqlBackend IO JobTemplates
readAndParseConfigFile id _ p = do
    testId <- entityKey . head <$> getPlaybooks (entityKey p) -- TODO: Remove, this is only for testing
    return $ M.fromList [("TestJob1", JobTemplate{_scheduleFormat=scheduleNext, _repoPath="ansible-example1", _playbook="pb.yml", _playbookId=testId, _failCount=0, _systemJob=False, _repoIdentifier=id})]

markProjectFailed :: Key Project -> String -> ReaderT SqlBackend IO ()
markProjectFailed key e = update key [ProjectErrorMessage =. e]

getRepo :: String -> String -> String -> String -> IO (Either String (Bool,String))
getRepo oid path url branch =
    ifM (liftIO (doesDirectoryExist path)) (updateFolder oid path url branch) (fillFolder path url branch)

updateFolder :: String -> String -> String -> String -> IO (Either String (Bool,String))
updateFolder oid path url branch =
    ifM (liftIO (isRepo path url))
        (doPullPerhaps oid path branch)
        (fillFolder path url branch)

doPullPerhaps :: String -> String -> String -> IO (Either String (Bool,String))
doPullPerhaps oid path branch = do
    pull <- doPull path branch
    oidNew <- getLastOid --TODO Ask Jonny if this is ok
    return $ pull <&> \_ -> if oidNew == oid then (False, oid)
                                             else (True, oidNew)

fillFolder :: String -> String -> String -> IO (Either String (Bool,String))
fillFolder path url branch = liftIO (removePathForcibly path) >> doClone url path branch >>= getOidAfterAction

getOidAfterAction :: Either String () -> IO (Either String (Bool,String))
getOidAfterAction retAction = getLastOid >>= \oid -> return $ (True,oid) <$ retAction

readJobQueueDatabase :: ReaderT SqlBackend IO JobTemplates
readJobQueueDatabase = do
    dataJoin <- joinJobQueuePlaybookProject >>= removeIllegalJobs
    let templs = map (\(_,x,y) -> createUserTemplate x y) dataJoin
    mapM_ (delete . entityKey . fst3) dataJoin
    return $ M.fromList $ zip [schedulerUserTemplateKey ++ show n | n <- [0..]] templs

createUserTemplate :: Entity Playbook -> Entity Project -> JobTemplate
createUserTemplate play proj =
    JobTemplate{_scheduleFormat=scheduleNext, _repoPath=projectToPath proj, _playbook=playbookPlaybookName $ entityVal play,
                _playbookId=entityKey play, _failCount=0, _systemJob=False, _repoIdentifier=""} --TODO: Add support for repoIdentifier

-- |Assumes ssh format (e.g. git@git.example.com:test/test-repo.git) and return the path (e.g. test/test-repo)
projectToPath :: Entity Project -> FilePath
projectToPath p = removeGitSuffix (after '/' (projectUrl (entityVal p))) ++ show (fromSqlKey (entityKey p))
    where
        removeGitSuffix = reverse . after '.' . reverse

getDatabaseJobQueue :: ReaderT SqlBackend IO [Entity JobQueue]
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
    jobTempls' <- runSqlPool (readJobQueueDatabase >>= \u -> M.union u <$> updateConfigRepoJobTemplates jobTempls) pool --Have different timing for the two updates
    snd <$> runStateT (calculateNextInstances >>= getDueJobs >>= executeJobs pool) jobTempls'

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t e = b >>= \b -> if b then t else e

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

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
