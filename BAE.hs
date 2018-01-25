{-# LANGUAGE ScopedTypeVariables #-}

module BAE where

import Budget (Interval(..))
import Data.Proxy (Proxy(..))
import Data.Reflection (reify)
import Data.Tagged (Tagged(..))
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime
import Data.Time.Recurrence as R
import Time

timeZoneBAE :: TimeZone
timeZoneBAE = TimeZone (-300) False "EST"

holidayTable :: [ZonedTime]
holidayTable =
    [ mkZonedTime timeZoneBAE 2018 1 1 9 0
    ]

data WorkHours
    = Holiday
    | Weekend
    | OffFriday
    | HalfFriday
    | RegularDay
    | NotWorking
    deriving (Eq, Show)

workHoursToInt :: Bool -> WorkHours -> Int
workHoursToInt _     NotWorking = 0
workHoursToInt _     Weekend    = 0
workHoursToInt _     Holiday    = 0
workHoursToInt _     OffFriday  = 0
workHoursToInt _     HalfFriday = 4
workHoursToInt False RegularDay = 9
workHoursToInt True  RegularDay = 8

twoWeekStart :: ZonedTime
twoWeekStart = mkZonedTime timeZoneBAE 2017 12 29 9 0

twoWeekDates :: [ZonedTime]
twoWeekDates = reify timeZoneBAE $ \(Proxy :: Proxy s) ->
    fmap unTagged
        . starting (Tagged  twoWeekStart :: Tagged s ZonedTime)
        $ recur (daily `by` 14)

baeTwoWeekRange :: ZonedTime -> (ZonedTime, ZonedTime)
baeTwoWeekRange moment =
    interval (setTimeZone timeZoneBAE moment) twoWeekDates
  where
    interval t (x:y:xs) | y > t     = (x, y)
                        | otherwise = interval t (y:xs)
    interval _ _ = error "impossible"

workIntervals :: Bool -> ZonedTime -> ZonedTime
              -> [Interval ZonedTime WorkHours]
workIntervals mine beg fin = reify (zonedTimeZone beg) $ \(Proxy :: Proxy s) ->
    concatMap go
        . takeWhile (< fin)
        . map unTagged
        . starting (Tagged beg :: Tagged s ZonedTime)
        $ recur daily >==> R.filter (WeekDays [Monday .. Friday])
  where
    go b | b == beg =
           [Interval b (addh 8 b) (if mine then RegularDay else OffFriday)]

         | b `elem` holidayTable =
           [Interval b (addh 9 b) Holiday]

         | otherwise =
           let (_, _, w) = toWeekDate (dayOf b)
           in case w of
                5 -> let mid = addh 4 b in
                    [ Interval b mid HalfFriday
                    , Interval mid (addh 4 mid) HalfFriday ]
                _ ->
                    [Interval b (addh (if mine then 8 else 9) b) RegularDay]

    addh n = addZonedTime (n * 3600)
