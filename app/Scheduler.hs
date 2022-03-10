{-# LANGUAGE TemplateHaskell #-}

module Scheduler where

import Ansible
import Data.Time.Compat
import Data.Time.Calendar.Compat
import Data.Time.LocalTime.Compat
import Data.Time.Clock.Compat
import Data.List (sort, intercalate)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Trans

data JobTemplate = JobTemplate {_scheduleFormat :: Schedule, _repoPath :: String, _playbook :: String, _failCount :: Int, _systemJob :: Bool}
data Job = Job {_timeDue :: LocalTime, _templateName :: String}
    deriving (Eq)
data Schedule = Schedule {_scheduleDay :: [DayOfWeek], _scheduleTime :: Maybe ScheduleTime}
data ScheduleTime = ScheduleTime {_startTime :: [TimeOfDay], _repetitionTime :: [Int]}

type JobTemplates = M.Map String JobTemplate
type Jobs = [Job]

instance Ord Job where
    compare j1 j2 = compare (_timeDue j1) (_timeDue j2)

makeLenses ''Job
makeLenses ''JobTemplate
makeLenses ''Schedule
makeLenses ''ScheduleTime

failMax = 3

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
    modify $ M.map removeUserJobTemplates <&> catMaybesMap
    return $ map (calculateNextInstance time) $ M.toList templates

catMaybesMap :: Ord k => M.Map k (Maybe v) -> M.Map k v
catMaybesMap m = M.fromList $ foldr (\(k,v) l -> case v of {Just v' -> (k,v'):l; Nothing -> l}) [] (M.toList m)

calculateNextInstance :: LocalTime -> (String,JobTemplate) -> Job
calculateNextInstance time (name,templ) = Job {_timeDue = dueTime (templ^.schedule.scheduleTime) daysThisWeek, _templateName = name}
    where
        daysThisWeek :: [Day]
        daysThisWeek = concatMap (weekStartingAt (localDay time)) $ templ^.schedule.scheduleDay
        dueTime :: Maybe ScheduleTime -> [Day] -> LocalTime
        dueTime st days = case st of
                            --                                             This are all possible LocalTimes
                            Just schedTime -> minimum $ filter (>= time) $ LocalTime <$> days <*> scheduleTimeToList schedTime
                            Nothing        -> LocalTime (days!!1) midnight

scheduleTimeToList :: ScheduleTime -> [TimeOfDay]
scheduleTimeToList = undefined

-- |Calculates the first week beginning on a certain day of the week after a certain day
weekStartingAt :: Day -> DayOfWeek -> [Day]
weekStartingAt startDay weekDay = take 7 $ iterate succ $ firstDayOfWeekOnAfter weekDay startDay

removeUserJobTemplates :: JobTemplate -> Maybe JobTemplate
removeUserJobTemplates jt = if jt^.systemJob then Just jt else Nothing

getDueJobs :: Jobs -> StateT JobTemplates IO Jobs
getDueJobs jobs = do
    time <- liftIO getTime
    return $ takeWhile (\j -> j^.timeDue <= time) (sort jobs)

executeJobs :: Jobs -> StateT JobTemplates IO ()
executeJobs = mapM_ executeJob

executeJob :: Job -> StateT JobTemplates IO ()
executeJob job = do
    template <- get
    when (maybe False (\x -> x ^. failCount <= failMax) (template ^? ix (job^.templateName))) $ do  -- TODO: Change to exec
        success  <- liftIO $ (==1) <$> ansiblePlaybook "../ansible" (template ^. ix (job^.templateName) . playbook) "" ""
        let success = False
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

schedule = undefined
