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

module ScheduleFormat (Schedule (..), ScheduleTime (..), nextInstance, scheduleNext, addTimeOfDay, parseScheduleFormat) where

import Data.Time.Calendar.Compat (DayOfWeek (..), Day, dayOfWeek, firstDayOfWeekOnAfter, addDays)
import Data.Time.LocalTime.Compat (TimeOfDay (..), LocalTime (..), dayFractionToTimeOfDay, midnight)
import Data.Functor ((<&>))
import Data.List (intercalate, nub)
import Parser
import Control.Lens.Combinators (makeLenses)
import Control.Lens (makeLensesFor, (^.), (^?!))
import Control.Applicative (Alternative ((<|>)))
import Text.Read (readMaybe)

data Schedule = Schedule {_scheduleDay :: [DayOfWeek], _scheduleTime :: Maybe ScheduleTime} | Now
data ScheduleTime =
    ScheduleTime { _startTime :: [TimeOfDay]
                 , _repetitionTime :: [TimeOfDay]
                 }

makeLenses ''Schedule
makeLenses ''ScheduleTime
makeLensesFor [("todMin", "todMinL"), ("todHour", "todHourL")] ''TimeOfDay

instance Show ScheduleTime where --TODO: Debugging purpose, remove or change!!
    show s = intercalate "," (map show (s^.startTime)) ++ "/" ++ intercalate "," (map show (s^.repetitionTime))

instance Show Schedule where --TODO: Debugging purpose, remove or change!!
    show Now = "Now"
    show s = intercalate "," (map show (s^.scheduleDay)) ++ " " ++ maybe [] show (s ^?! scheduleTime)

allFullHours :: [TimeOfDay]
allFullHours = take 24 $ iterate (addTimeOfDay (dayFractionToTimeOfDay (1/24))) midnight

fullWeek :: [DayOfWeek]
fullWeek = enumFromTo Monday Sunday

scheduleNext :: Schedule
scheduleNext = Schedule{_scheduleDay=fullWeek, _scheduleTime=Just ScheduleTime{_startTime=allFullHours, _repetitionTime=[TimeOfDay{todHour=0,todMin=1,todSec=0}]}}


-- /PARSER/ --

-- '_' means space here
-- <Top>      ::= <Weekdays> | <Weeksdays> _ <Times> | <Times>
-- <Weekdays> ::= <List(Enum(Day))>
-- <List(A)>  ::= A | A , <List(A)>
-- <Enum(A)>  ::= <A> .. <Enum(A)> | <A>
-- <Day>      ::= mon | tue | wed | thu | fri | sat | sun
-- <Times>    ::= <List(Time0)> | / <List(Time1)> | <List(Time0)> / <List(Time1)>
-- <Time1>    ::= <Hour> : <Min> | <Min1>
-- <Time0>    ::= <Hour> : <Min> | <Min0>
-- <HourSym>  ::= 00 | 01 .. 24
-- <Min>      ::= 00 | 01 .. 60
-- <Min1>     ::= 1  |  2 .. 60
-- <Min0>     ::= 0  |  2 .. 60

parseList :: Eq a => Parser a -> Parser [a]
parseList p = parseList' p $ parseList p

parseList' :: Eq a => Parser a -> Parser [a] -> Parser [a]
parseList' p p' = (:[]) <$> p
              <|> (:) <$> p <*> (char ',' *> p')

parseScheduleFormat :: String -> Maybe Schedule
parseScheduleFormat = parse parseSchedule

parseSchedule :: Parser Schedule
parseSchedule = flip Schedule Nothing <$> parseWeekdays
            <|> Schedule fullWeek . Just <$> parseTimes
            <|> (\w t -> Schedule w (Just t)) <$> parseWeekdays <*> (char ' ' *> parseTimes)

parseEnum :: Eq a => Enum a => Parser a -> Parser [a]
parseEnum p = (:[]) <$> p
          <|> (enumFromTo <$> p <*> (keyword ".." *> p))

parseWeekdays :: Parser [DayOfWeek]
parseWeekdays = concat <$> parseList (parseEnum parseDay)

parseDay :: Parser DayOfWeek
parseDay = Monday    <$ keyword "mon"
       <|> Tuesday   <$ keyword "tue"
       <|> Wednesday <$ keyword "wed"
       <|> Thursday  <$ keyword "thu"
       <|> Friday    <$ keyword "fri"
       <|> Saturday  <$ keyword "sat"
       <|> Sunday    <$ keyword "sun"

-- | Parses @[[start-time(s)][/repetition-time(s)]]@
parseTimes :: Parser ScheduleTime
parseTimes = ScheduleTime <$> parseStart <*> (char '/' *> parseRepetition)
         <|> flip ScheduleTime [TimeOfDay 0 1 0] <$> parseStart
         <|> ScheduleTime allFullHours <$> (char '/' *> parseRepetition)

-- | Parses @[start-time(s)]@
parseStart :: Parser [TimeOfDay]
parseStart = nub <$> ((++) <$> parseStartMinutes <*> (char ',' *> parseStart))
         <|> nub <$> parseStartMinutes

-- | Parses the minutes of the start-time
-- Allows that the hours and @:@ are missing
parseStartMinutes :: Parser [TimeOfDay]
parseStartMinutes = (\m -> map (`addTimeOfDay` TimeOfDay 0 m 0) allFullHours) <$> parseMin' 0
                <|> (:[]) <$> parseTimeOfDay

-- | Parses @[repetition-time(s)]@
parseRepetition :: Parser [TimeOfDay]
parseRepetition = nub <$> parseList' parseRepetitionMinutes parseRepetition
              <|> (:[]) <$> parseRepetitionMinutes

-- | Parses the minutes of the repetition-time
-- Allows that the hours and @:@ are missing
parseRepetitionMinutes :: Parser TimeOfDay
parseRepetitionMinutes = (\m -> TimeOfDay 0 m 0) <$> parseMin' 1
                     <|> parseTimeOfDay

-- | Parses a time-of-day without the seconds (@hh:mm@)
parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = TimeOfDay <$> parseHour <*> (char ':' *> parseMin) <*> pure 0

parseHour :: Parser Int
parseHour = parseBetween 0 24

-- | Parses a minute, where the length of the digits is two (filled with leading zeros)
parseMin :: Parser Int
parseMin = parseBetween 0 60

-- | Parses a minute, where the digits don't have a leading zeros and are >= than the argument
parseMin' :: Int -> Parser Int
parseMin' i = helper 1 <|> helper 2
    where helper n = Parser $ \s -> let (m,r) = splitAt n s
                                    in case readMaybe m of
                                         Just int -> [(int,r) | i <= int && int < 60]
                                         _        -> []

-- | Parses a int greater equal than the lower und smaller than the upper bound
-- Furthermore, expects the length of the input to be as big as the length of the upper bound
-- @parse (parseBetween 0 60) "5"  == Nothing@
-- @parse (parseBetween 0 60) "05" == Just 5@
parseBetween :: Int -> Int -> Parser Int
parseBetween l u = Parser $ \s -> let (m,r) = splitAtExactly (length (show u)) s
                                  in case readMaybe m of
                                       Just int -> [(int,r) | l <= int && int < u]
                                       _        -> []

-- \PARSER\ --


-- /INTERPRETOR/ --

-- | Given a time and a schedule expression, calculates the soonest time that matches the expression
nextInstance :: LocalTime -> Schedule -> LocalTime
nextInstance time Now = time
nextInstance time schedule = minimum $ filter (>= time) allPossibleTimes
    where
        allPossibleTimes = LocalTime <$> instanceOfDaysOfWeek (localDay time) (schedule^.scheduleDay)
                                     <*> case schedule ^?! scheduleTime of
                                           Nothing -> pure midnight
                                           Just s  -> scheduleTimeToList s

-- | Transforms a list of generic weekdays (e.g. @[Monday]@) to a list specific ones (e.g. @[Monday 13.03.2022]@).
-- These are the first occurences of the weekdays on and after a certain day.
-- If the weekday of the given day is part of the list, it will be added twice, one time the day itself and the other time seven days plus the day
instanceOfDaysOfWeek :: Day -> [DayOfWeek] -> [Day]
instanceOfDaysOfWeek d ds = prependBool (dayOfWeek d `elem` ds) (firstDayOfWeekAfter (dayOfWeek d) d)
    $ map (`firstDayOfWeekOnAfter` d) ds

-- |Calculates all possible time-of-days that match the schedule-time expression
scheduleTimeToList :: ScheduleTime -> [TimeOfDay]
scheduleTimeToList st = concat [ iterateN (maxMultiple start rep) (addTimeOfDay rep) start
                               | start <- st^.startTime, rep <- st^.repetitionTime ]

-- | For parameters @t r@, calculates how many times one can add @r@ to @t@ until the next hour/day (@r@ consits only of minutes/@r@ has non zero hour) is reached.
maxMultiple :: TimeOfDay -> TimeOfDay -> Int
maxMultiple = maxMultipleH 1
    where maxMultipleH n time mult =
            case mult^.todHourL of
              0 -> if time^.todMinL + mult^.todMinL < 60 then maxMultipleH (n+1) (addTimeOfDay time mult) mult else n
              x -> let hours = time^.todHourL + mult^.todHourL + ((time^.todMinL + mult^.todMinL) `div` 60) in
                   if hours < 24 then maxMultipleH (n+1) (addTimeOfDay time mult) mult else n

-- | Addes two time-of-days ignoring the seconds
addTimeOfDay :: TimeOfDay -> TimeOfDay -> TimeOfDay
addTimeOfDay TimeOfDay{todHour=t1h, todMin=t1m} TimeOfDay{todHour=t2h, todMin=t2m} =
    TimeOfDay{todHour=(t1h+t2h + ((t1m+t2m) `div` 60)) `mod` 24, todMin=(t1m+t2m) `mod` 60, todSec=0}

-- | The first day-of-week after some day
firstDayOfWeekAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekAfter weekday day = firstDayOfWeekOnAfter weekday (addDays 1 day)

-- \INTERPRETOR\ --


-- /MISC/ --

prependBool :: Bool -> a -> [a] -> [a]
prependBool b x xs = if b then x:xs else xs

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = take n $ iterate f x

-- | @splitAtExactly 2 "12:00" == ("12","00")@, but @splitAtExactly 2 "1" == ("","1")@
splitAtExactly :: Int -> [a] -> ([a],[a])
splitAtExactly n list = if length l < n then ([], list) else (l,r)
    where (l,r) = splitAt n list

-- \MISC\ --
