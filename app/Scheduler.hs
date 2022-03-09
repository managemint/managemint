{-# LANGUAGE TemplateHaskell #-}

module Scheduler where

import Ansible
import Data.Time.Clock
import Data.List (sort, intercalate)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans

data JobTemplate = JobTemplate {_scheduleFormat :: Schedule, _playbook :: String , _optArgs:: [String], _failCount :: Int, _systemJob :: Bool}
data Job = Job {_timeDue :: UTCTime, _templateName :: String}
    deriving (Eq)
data Schedule

type JobTemplates = M.Map String JobTemplate
type Jobs = [Job]

instance Ord Job where
    compare j1 j2 = compare (_timeDue j1) (_timeDue j2)

makeLenses ''Job
makeLenses ''JobTemplate

calculateNextInstances :: StateT JobTemplates IO Jobs
calculateNextInstances = do
    templates <- get
    time <- liftIO getCurrentTime
    modify $ M.map removeUserJobTemplates <&> catMaybesMap
    return $ sort $ map (calculateNextInstance time) $ M.toList templates

catMaybesMap :: Ord k => M.Map k (Maybe v) -> M.Map k v
catMaybesMap m = M.fromList $ foldr (\(k,v) l -> case v of {Just v' -> (k,v'):l; Nothing -> l}) [] (M.toList m)

calculateNextInstance :: UTCTime -> (String,JobTemplate) -> Job
calculateNextInstance = undefined

removeUserJobTemplates :: JobTemplate -> Maybe JobTemplate
removeUserJobTemplates jt = if jt^.systemJob then Just jt else Nothing

getDueJobs :: Jobs -> StateT JobTemplates IO Jobs
getDueJobs jobs = do
    time <- liftIO getCurrentTime
    return $ takeWhile (\j -> j^.timeDue <= time) (sort jobs)

executeJobs :: Jobs -> StateT JobTemplates IO ()
executeJobs = mapM_ executeJob

-- TODO: Dont execute Job if failcount exeeds a certain value
executeJob :: Job -> StateT JobTemplates IO ()
executeJob job = do
    template <- get
    success  <- liftIO $ (==1) <$> ansiblePlaybook "../ansible" (template ^. ix (job^.templateName) . playbook) (intercalate "," (template ^. ix (job^.templateName) . optArgs)) ""
    put $ template & ix (job^.templateName) %~ (& failCount %~ modi success)
        where
            modi :: Bool -> (Int -> Int)
            modi True  = const 0
            modi False = (+1)

updateConfigRepoJobTemplates :: JobTemplates -> IO JobTemplates
updateConfigRepoJobTemplates = undefined

readJobsDatabase :: IO JobTemplates
readJobsDatabase = undefined

-- |Given a list of initial job templates (the ones from the config repo), updates them, reads the user jobs from the databse and executes all due ones
runJobs :: JobTemplates -> IO JobTemplates
runJobs jobTempls = do
    jobTempls' <- mappend <$> updateConfigRepoJobTemplates jobTempls <*> readJobsDatabase    -- somehow (++) doesn't work, therefore I used mappend
    snd <$> runStateT (calculateNextInstances >>= getDueJobs >>= executeJobs) jobTempls'
