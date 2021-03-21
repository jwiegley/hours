{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Work (workIntervals, timeZoneWork) where

import Control.Arrow ((&&&))
import Data.Time hiding (DayOfWeek(..))
import Data.Time.Recurrence as R
import Hours.Budget (Interval(..))
import Hours.Input (WorkDay(..))
import Hours.Time

timeZoneWork :: TimeZone
timeZoneWork = TimeZone (-480) False "PST"

holidayTable :: [UTCTime]
holidayTable =
    [ zonedTimeToUTC (mkZonedTime timeZoneWork 2018  1  1 9 0)
    , zonedTimeToUTC (mkZonedTime timeZoneWork 2018  9  3 9 0)
    ]

workHours :: Bool -> WorkDay -> NominalDiffTime
workHours _ NotWorking = 0
workHours _ Weekend    = 0
workHours _ Holiday    = 0
workHours _ OffFriday  = 0
workHours _ HalfFriday = fromHours 1000
workHours _ RegularDay = fromHours 8

monthlyStart :: UTCTime
monthlyStart = zonedTimeToUTC (mkZonedTime timeZoneWork 2018 08 1 9 0)

startDate :: UTCTime
startDate = zonedTimeToUTC (mkZonedTime timeZoneWork 2018 08 13 9 0)

workRange :: UTCTime -> (UTCTime, UTCTime)
workRange = interval . starting monthlyStart $ recur monthly
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
        . starting (max beg startDate)
        $ recur daily >==> R.filter (WeekDays [Monday .. Friday])
  where
    (beg, fin) = workRange moment

    go b | b `elem` holidayTable = [ Interval b (addHours 8 b) Holiday    ]
         | otherwise        = [ Interval b (addHours 8 b) RegularDay ]
