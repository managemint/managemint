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

module ScheduleFormat (Schedule (..), ScheduleTime (..), nextInstance, allFullHours, fullWeek) where

import Data.Time.Compat
import Data.Time.Calendar.Compat
import Data.Time.LocalTime.Compat
import Data.Time.Clock.Compat
import Data.Functor ((<&>))
import Data.List (intercalate)
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

instance Show ScheduleTime where --TODO: Debugging purpose, remove or change!!
    show s = intercalate "," (map show (s^.startTime)) ++ "/" ++ intercalate "," (map show (s^.repetitionTime))

instance Show Schedule where --TODO: Debugging purpose, remove or change!!
    show s = intercalate "," (map show (s^.scheduleDay)) ++ " " ++ maybe [] show (s^.scheduleTime)

allFullHours :: [TimeOfDay]
allFullHours = take 12 $ iterate (addTimeOfDay (dayFractionToTimeOfDay (1/24))) midnight

fullWeek :: [DayOfWeek]
fullWeek = enumFromTo Monday Sunday

parseDaysFormat :: String -> Maybe [DayOfWeek]
parseDaysFormat "" = Just $ enumFromTo Monday Sunday
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
nextInstance time schedule = minimum $ filter (>= time) allPossibleTimes
    where
        allPossibleTimes = LocalTime <$> instanceOfDaysOfWeek (localDay time) (schedule^.scheduleDay)
                                     <*> case schedule^.scheduleTime of
                                           Nothing -> pure midnight
                                           Just s  -> scheduleTimeToList s

-- |Transforms a list of generic weekdays (e.g. [Monday]) to a list specific ones (e.g. [Monday 13.03.2022]).
--These are the first occurences of the weekdays on and after a certain day.
--If the weekday of the given day is part of the list, it will be added twice, one time the day itself and the other time seven days plus the day
instanceOfDaysOfWeek :: Day -> [DayOfWeek] -> [Day]
instanceOfDaysOfWeek d ds = prependBool (dayOfWeek d `elem` ds) (firstDayOfWeekAfter (dayOfWeek d) d)
    $ map (`firstDayOfWeekOnAfter` d) ds

-- |Calculates all possible TimeOfDays that match the ScheduleTime expression
scheduleTimeToList :: ScheduleTime -> [TimeOfDay]
scheduleTimeToList st = concat [ iterateN (maxMultiple start rep) (addTimeOfDay rep) start
                               | start <- st^.startTime, rep <- st^.repetitionTime ]

-- |For parameters t r, calculates how many times can one add r to t until the next hour/day (r consits only of minutes/r has non zero hour) is reached.
maxMultiple :: TimeOfDay -> TimeOfDay -> Int
maxMultiple = maxMultipleH 1
    where maxMultipleH n time mult =
            case mult^.todHourL of
              0 -> if time^.todMinL + mult^.todMinL < 60 then maxMultipleH (n+1) (addTimeOfDay time mult) mult else n
              x -> let hours = time^.todHourL + mult^.todHourL + ((time^.todMinL + mult^.todMinL) `div` 60) in
                   if hours < 24 then maxMultipleH (n+1) (addTimeOfDay time mult) mult else n

-- |Addes two TimeOfDays ignoring the seconds
addTimeOfDay :: TimeOfDay -> TimeOfDay -> TimeOfDay
addTimeOfDay TimeOfDay{todHour=t1h, todMin=t1m} TimeOfDay{todHour=t2h, todMin=t2m} =
    TimeOfDay{todHour=(t1h+t2h + ((t1m+t2m) `div` 60)) `mod` 24, todMin=(t1m+t2m) `mod` 60, todSec=0}

firstDayOfWeekAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekAfter weekday day = firstDayOfWeekOnAfter weekday (addDays 1 day)

prependBool :: Bool -> a -> [a] -> [a]
prependBool b x xs = if b then x:xs else xs

-- |breakOn '=' "x=1" == ("x","1")
breakOn :: Eq a => a -> [a] -> ([a],[a])
breakOn x = break (/= x)

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = take n $ iterate f x
