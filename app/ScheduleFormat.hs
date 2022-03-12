{- app/SchedulerFormat.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE TemplateHaskell #-}

module ScheduleFormat (Schedule (..), nextInstance) where

import Data.Time.Compat
import Data.Time.Calendar.Compat
import Data.Time.LocalTime.Compat
import Data.Time.Clock.Compat
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Containers.ListUtils
import Control.Lens
import Text.Read

data Schedule = Schedule {_scheduleDay :: [DayOfWeek], _scheduleTime :: Maybe ScheduleTime}
data ScheduleTime =
    ScheduleTime { _startTime :: [TimeOfDay]
                 , _repetitionTime :: [TimeOfDay]
                 }

-- Für Parser:
--   Format: [day(s)] [[start-time(s)][/repetition-time(s)]]
--     Wenn [/repetition-time(s)] nicht gegeben ist, lese alle 24h ein
--     Sodersyntax schon beim Parsen auflößen, z.B. *:00 zu 0:00,1:00,...,23:00
-- Idee: splitOn ' ' und '/' und seperat parsen

makeLenses ''Schedule
makeLenses ''ScheduleTime
makeLensesFor [("todMin", "todMinL"), ("todHour", "todHourL")] ''TimeOfDay

allFullHours :: [TimeOfDay]
allFullHours = take 12 $ iterate (addTimeOfDay (dayFractionToTimeOfDay (1/24))) midnight

-- |breakOn '=' "x=1" == ("x","1")
breakOn :: Eq a => a -> [a] -> ([a],[a])
breakOn x = break (/= x)

parseDaysFormat :: String -> Maybe [DayOfWeek]
parseDaysFormat ds = nubOrd . concat <$> mapM parseEnumDays (splitOn "," ds)
    where
        parseEnumDays :: String -> Maybe [DayOfWeek]
        parseEnumDays s = case splitOn ".." s of
                         [x]   -> parseDayOfWeek x <&> (:[])
                         [l,r] -> enumFromTo <$> parseDayOfWeek l <*> parseDayOfWeek r
                         _     -> Nothing

parseDayOfWeek :: String -> Maybe DayOfWeek
parseDayOfWeek s = case s of
                     "mon" -> Just Monday
                     "tue" -> Just Tuesday
                     "wen" -> Just Wednesday
                     "thu" -> Just Thursday
                     "fri" -> Just Friday
                     "sat" -> Just Saturday
                     "sun" -> Just Sunday
                     _     -> Nothing

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
