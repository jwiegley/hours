{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hours.Time where

import Data.Time
import Hours.Budget (HasDelta(..), Scalable(..))

instance HasDelta UTCTime NominalDiffTime where
    delta = diffUTCTime

instance Scalable NominalDiffTime NominalDiffTime where
    scale = (*)

mkZonedTime :: TimeZone -> Integer -> Int -> Int -> Int -> Int -> ZonedTime
mkZonedTime tz y m d hh mm =
    ZonedTime (LocalTime (fromGregorian y m d) (TimeOfDay hh mm 0)) tz

diffTimeZone :: TimeZone -> TimeZone -> NominalDiffTime
diffTimeZone x y = fromIntegral (timeZoneMinutes x - timeZoneMinutes y) * 60

addHours :: Int -> UTCTime -> UTCTime
addHours = addUTCTime . fromHours

setHour :: Int -> UTCTime -> UTCTime
setHour hour now = UTCTime (utctDay now) (fromIntegral hour * 3600)

toHours :: NominalDiffTime -> Double
toHours x = realToFrac x / 3600.0 :: Double

fromHours :: Int -> NominalDiffTime
fromHours x = fromIntegral (x * 3600)

parseIso :: Monad m => String -> m UTCTime
parseIso = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %z"

formatIso :: UTCTime -> String
formatIso = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z"
