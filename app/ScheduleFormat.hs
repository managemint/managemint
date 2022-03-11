{-# LANGUAGE TemplateHaskell #-}

module ScheduleFormat where

import Data.Time.Compat
import Data.Time.Calendar.Compat
import Data.Time.LocalTime.Compat
import Data.Time.Clock.Compat
import Control.Lens

data Schedule = Schedule {_scheduleDay :: [DayOfWeek], _scheduleTime :: Maybe ScheduleTime}
data ScheduleTime = 
    ScheduleTime { _startTime :: [TimeOfDay]
                 , _repetitionTime :: [TimeOfDay]
                 }


-- Für Parser:
--   Format: [day(s)] [[start-time(s)][/repetition-time(s)]]
--     Wenn [/repetition-time(s)] nicht gegeben ist, lese alle 24h ein
--     Sodersyntax schon beim Parsen auflößen, z.B. *:00 zu 0:00,1:00,...,23:00


makeLenses ''Schedule
makeLenses ''ScheduleTime
makeLensesFor [("todMin", "todMinL"), ("todHour", "todHourL")] ''TimeOfDay

-- |Given a time and a Schedule expression, calculates the soonest time that matches the expression
nextInstance :: LocalTime -> Schedule -> LocalTime
nextInstance time schedule = dueTime (schedule^.scheduleTime) daysThisWeek
    where
        daysThisWeek :: [Day]
        daysThisWeek = concatMap (weekStartingAt (localDay time)) $ schedule^.scheduleDay
        dueTime :: Maybe ScheduleTime -> [Day] -> LocalTime
        dueTime st days = case st of
                                                                        -- These are all possible LocalTimes
                            Just schedTime -> minimum $ filter (>= time) $ LocalTime <$> days <*> scheduleTimeToList schedTime
                            Nothing        -> LocalTime (days!!1) midnight

-- |Calculates all possible TimeOfDays that match the ScheduleTime expression
scheduleTimeToList :: ScheduleTime -> [TimeOfDay]
scheduleTimeToList st = concat [ takeWhile (<= maxTime start rep) $ iterate (addTimeOfDayStupid rep) start
                               | start <- st^.startTime, rep <- st^.repetitionTime ]
    where
        maxTime :: TimeOfDay -> TimeOfDay -> TimeOfDay
        maxTime time rep = case rep^.todHourL of
                             0 -> if time^.todMinL + rep^.todMinL < 60 then maxTime (time & todMinL  %~ (+rep^.todMinL)) rep else time
                             x -> if time^.todHourL + x < 24 then maxTime (addTimeOfDay time rep) rep else time
        addTimeOfDayStupid :: TimeOfDay -> TimeOfDay -> TimeOfDay
        addTimeOfDayStupid TimeOfDay{todHour=t1h, todMin=t1m} TimeOfDay{todHour=t2h, todMin=t2m} =
            TimeOfDay{todHour=t1h+t2h + ((t1m+t2m) `div` 60), todMin=(t1m+t2m) `mod` 60, todSec=0}

-- |Addes two TimeOfDays ignoring the seconds
addTimeOfDay :: TimeOfDay -> TimeOfDay -> TimeOfDay
addTimeOfDay TimeOfDay{todHour=t1h, todMin=t1m} TimeOfDay{todHour=t2h, todMin=t2m} =
    TimeOfDay{todHour=(t1h+t2h + ((t1m+t2m) `div` 60)) `mod` 24, todMin=(t1m+t2m) `mod` 60, todSec=0}

-- |Calculates the first week beginning on a certain day of the week after a certain day
weekStartingAt :: Day -> DayOfWeek -> [Day]
weekStartingAt startDay weekDay = take 7 $ iterate succ $ firstDayOfWeekOnAfter weekDay startDay
