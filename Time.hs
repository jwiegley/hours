{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Time where

import qualified Budget
import           Data.Proxy (Proxy(..))
import           Data.Reflection (Reifies(..))
import           Data.Tagged (Tagged(..))
import           Data.Time.Calendar (Day, toGregorian, fromGregorian)
import           Data.Time.CalendarTime (CalendarTimeConvertible(..))
import           Data.Time.Clock (NominalDiffTime, diffUTCTime, addUTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.LocalTime
import           Data.Time.Moment (Moment(..))

instance Eq ZonedTime where
    x == y = zonedTimeToUTC x == zonedTimeToUTC y

instance Ord ZonedTime where
    compare x y = compare (zonedTimeToUTC x) (zonedTimeToUTC y)

instance Reifies s TimeZone => Moment (Tagged s ZonedTime) where
  epoch      = Tagged (utcToZonedTime (reflect (Proxy :: Proxy s))
                                      (posixSecondsToUTCTime 0))
  addSeconds (Tagged t) d = Tagged (addZonedTime (fromIntegral d) t)
  addMonths  = error "NYI: addMonths"
  addYears   = error "NYI: addYears"

instance Reifies s TimeZone =>
    CalendarTimeConvertible (Tagged s ZonedTime) where
  toCalendarTime (Tagged z) = toCalendarTime z
  fromCalendarTime t = Tagged <$> fromCalendarTime t

instance Budget.HasDelta ZonedTime NominalDiffTime where
    delta = diffZonedTime

instance Budget.Scalable NominalDiffTime NominalDiffTime where
    scale = (*)

diffTimeZone :: TimeZone -> TimeZone -> Int
diffTimeZone x y = (timeZoneMinutes x - timeZoneMinutes y) * 60

mkZonedTime :: TimeZone -> Integer -> Int -> Int -> Int -> Int -> ZonedTime
mkZonedTime tz y m d hh mm =
    ZonedTime (LocalTime (fromGregorian y m d) (TimeOfDay hh mm 0)) tz

diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime x y = diffUTCTime (zonedTimeToUTC x)
                                (zonedTimeToUTC y)

addZonedTime :: Int -> ZonedTime -> ZonedTime
addZonedTime x y = utcToZonedTime (zonedTimeZone y)
                                  (addUTCTime (fromIntegral x)
                                              (zonedTimeToUTC y))

setTimeZone :: TimeZone -> ZonedTime -> ZonedTime
setTimeZone tz = utcToZonedTime tz . zonedTimeToUTC

setHour :: Int -> ZonedTime -> ZonedTime
setHour hour now =
    mkZonedTime (zonedTimeZone now) (fromIntegral yr)
                (fromIntegral mon) (fromIntegral day) hour 0
  where
    (yr, mon, day) = toGregorian (dayOf now)

dayOf :: ZonedTime -> Day
dayOf = localDay . zonedTimeToLocalTime

timeOf :: ZonedTime -> TimeOfDay
timeOf = localTimeOfDay . zonedTimeToLocalTime

toHours :: NominalDiffTime -> Double
toHours x = realToFrac x / 3600.0 :: Double

fromHours :: Int -> NominalDiffTime
fromHours x = fromIntegral (x * 3600)
