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

    weekDate = thrd . toWeekDate . localDay . utcToLocalTime timeZoneBAE
      where thrd (_, _, x) = x

    go b | b == beg, mine       = [ Interval b (addHours 8 b) RegularDay ]
         | b == beg             = [ Interval b (addHours 8 b) OffFriday  ]
         | b `elem` holidayTable     = [ Interval b (addHours 9 b) Holiday    ]
         | 5 <- weekDate b, mine = [ Interval b (addHours 8 b) RegularDay ]
         | 5 <- weekDate b       = let mid = addHours 4 b in
                                  [ Interval b mid HalfFriday
                                  , Interval mid (addHours 4 mid) HalfFriday ]
         | mine                 = [ Interval b (addHours 8 b) RegularDay ]
         | otherwise            = [ Interval b (addHours 9 b) RegularDay ]
