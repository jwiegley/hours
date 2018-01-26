{-# LANGUAGE ScopedTypeVariables #-}

module BAE where

import Budget (Interval(..))
import Data.Proxy (Proxy(..))
import Data.Reflection (reify)
import Data.Tagged (Tagged(..))
import Data.Time (NominalDiffTime)
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

data WorkDay
    = NotWorking
    | Holiday
    | Weekend
    | OffFriday
    | HalfFriday
    | RegularDay
    deriving (Eq, Show)

workHours :: Bool -> WorkDay -> NominalDiffTime
workHours _     NotWorking = 0
workHours _     Weekend    = 0
workHours _     Holiday    = 0
workHours _     OffFriday  = 0
workHours _     HalfFriday = fromHours 4
workHours False RegularDay = fromHours 9
workHours True  RegularDay = fromHours 8

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
              -> [Interval ZonedTime (WorkDay, NominalDiffTime)]
workIntervals mine beg fin = reify (zonedTimeZone beg) $ \(Proxy :: Proxy s) ->
    map (fmap (\x -> (x, workHours mine x)))
        . concatMap go
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
