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
data ScheduleTime = ScheduleTime {_startTime :: [TimeOfDay], _repetitionTime :: [TimeOfDay]}

type JobTemplates = M.Map String JobTemplate
type Jobs = [Job]

instance Ord Job where
    compare j1 j2 = compare (_timeDue j1) (_timeDue j2)

makeLenses ''Job
makeLenses ''JobTemplate
makeLenses ''Schedule
makeLenses ''ScheduleTime
makeLensesFor [("todMin", "todMinL"), ("todHour", "todHourL")] ''TimeOfDay

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
    modify $ M.filter (^. systemJob)
    return $ map (calculateNextInstance time) $ M.toList templates

calculateNextInstance :: LocalTime -> (String,JobTemplate) -> Job
calculateNextInstance time (name,templ) = Job {_timeDue = dueTime (templ^.schedule.scheduleTime) daysThisWeek, _templateName = name}
    where
        daysThisWeek :: [Day]
        daysThisWeek = concatMap (weekStartingAt (localDay time)) $ templ^.schedule.scheduleDay
        dueTime :: Maybe ScheduleTime -> [Day] -> LocalTime
        dueTime st days = case st of
                                                                        -- These are all possible LocalTimes
                            Just schedTime -> minimum $ filter (>= time) $ LocalTime <$> days <*> scheduleTimeToList schedTime
                            Nothing        -> LocalTime (days!!1) midnight

scheduleTimeToList :: ScheduleTime -> [TimeOfDay]
scheduleTimeToList st = concat [ takeWhile (<= maxTime start rep) $ iterate (addTimeOfDay rep) start
                               | start <- st^.startTime, rep <- st^.repetitionTime ]
    where
        maxTime :: TimeOfDay -> TimeOfDay -> TimeOfDay
        maxTime time rep = case rep^.todHourL of
                             0 -> if time^.todMinL + rep^.todMinL < 60 then maxTime (time & todMinL  %~ (+rep^.todMinL)) rep else time  -- Semms correct
                             x -> if time^.todHourL + x < 24 then maxTime (addTimeOfDay time rep) rep else time  -- TODO: Fix bug

-- |Addes two TimeOfDays ignoring the seconds
addTimeOfDay :: TimeOfDay -> TimeOfDay -> TimeOfDay
addTimeOfDay TimeOfDay{todHour=t1h, todMin=t1m} TimeOfDay{todHour=t2h, todMin=t2m} =
    TimeOfDay{todHour=(t1h+t2h + ((t1m+t2m) `div` 60)) `mod` 24, todMin=(t1m+t2m) `mod` 60, todSec=0}

-- |Calculates the first week beginning on a certain day of the week after a certain day
weekStartingAt :: Day -> DayOfWeek -> [Day]
weekStartingAt startDay weekDay = take 7 $ iterate succ $ firstDayOfWeekOnAfter weekDay startDay

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
        put $ template & ix (job^.templateName) %~ (& failCount %~ if success then const 0 else (+1))

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
