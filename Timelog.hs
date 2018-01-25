{-# LANGUAGE ViewPatterns #-}

module Timelog where

import Budget (Interval(..))
import Data.Char (toLower)
import Data.Foldable (Foldable(foldl'))
import Data.List (sortOn)
import Data.Maybe (isJust, fromMaybe)
import Data.Semigroup (Semigroup((<>)))
import Data.Time (defaultTimeLocale)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format (parseTimeM)
import Data.Time.LocalTime
import Time

parseTimeClockEntry :: TimeZone -> String -> (Bool, ZonedTime)
parseTimeClockEntry zone s = case words s of
    "i"                 : d : t : _ -> (True,  parseIso (d <> " " <> t))
    (map toLower -> "o") : d : t : _ -> (False, parseIso (d <> " " <> t))
    _ -> error $ "Invalid timeclock line: '" <> s <> "'"
  where
    parseIso t = fromMaybe
        (error ("Failed to parse time: " ++ t))
        (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%z"
             (t ++ timeZoneOffsetString zone))

parseLogbook :: ZonedTime -> String
             -> (Bool, [Interval ZonedTime NominalDiffTime])
parseLogbook now s = (isJust st, sortOn Budget.begin ints')
  where
    (st, ints) = foldl' go (Nothing, []) entries

    entries = map (parseTimeClockEntry (zonedTimeZone now)) (lines s)

    ints' = case st of
        Nothing -> ints
        Just i  -> Interval i now (diffZonedTime now i) : ints

    go (mbeg, xs) (isIn, x) = case (isIn, mbeg, x) of
        (True, Just _, _)   -> error "Already clocked in"
        (False, Nothing, _) -> error "Nothing to clock out of"
        (True, Nothing, v)  -> (Just v, xs)
        (False, Just i, o)  -> (Nothing, Interval i o (diffZonedTime o i) : xs)
