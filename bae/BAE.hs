{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BAE (workIntervals, timeZoneBAE) where

import Control.Arrow ((&&&))
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Recurrence as R
import Hours.Budget (Interval(..))
import Hours.Input (WorkDay(..))
import Hours.Time

holidayTable :: [UTCTime]
holidayTable =
    [ zonedTimeToUTC (mkZonedTime timeZoneBAE 2018 1 1 9 0)
    ]

timeZoneBAE :: TimeZone
timeZoneBAE = TimeZone (-300) False "EST"

workHours :: Bool -> WorkDay -> NominalDiffTime
workHours _     NotWorking = 0
workHours _     Weekend    = 0
workHours _     Holiday    = 0
workHours _     OffFriday  = 0
workHours _     HalfFriday = fromHours 4
workHours False RegularDay = fromHours 9
workHours True  RegularDay = fromHours 8

twoWeekStart :: UTCTime
twoWeekStart = zonedTimeToUTC (mkZonedTime timeZoneBAE 2017 12 29 9 0)

baeTwoWeekRange :: UTCTime -> (UTCTime, UTCTime)
baeTwoWeekRange = interval . starting twoWeekStart $ recur (daily `by` 14)
  where
    interval (x:y:xs) t | y > t     = (x, y)
                        | otherwise = interval (y:xs) t
    interval _ _ = error "impossible"

workIntervals :: Bool -> UTCTime
              -> [Interval UTCTime (WorkDay, NominalDiffTime)]
workIntervals mine moment =
    map (fmap (id &&& workHours mine))
        . concatMap go
        . takeWhile (< fin)
        . starting beg
        $ recur daily >==> R.filter (WeekDays [Monday .. Friday])
  where
    (beg, fin) = baeTwoWeekRange moment

    go b | b == beg =
           [Interval b (addHours 8 b)
                     (if mine then RegularDay else OffFriday)]

         | b `elem` holidayTable =
           [Interval b (addHours 9 b) Holiday]

         | otherwise =
           let localTime = utcToLocalTime timeZoneBAE b
               (_, _, w) = toWeekDate (localDay localTime)
           in case w of
                5 -> let mid = addHours 4 b in
                    if mine
                    then [Interval b (addHours 8 b) RegularDay]
                    else [ Interval b mid HalfFriday
                         , Interval mid (addHours 4 mid) HalfFriday ]
                _ ->
                    [Interval b (addHours (if mine then 8 else 9) b)
                              RegularDay]
