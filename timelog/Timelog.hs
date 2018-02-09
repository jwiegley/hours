{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Timelog where

import Control.Monad.State
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup((<>)))
import Data.Time (defaultTimeLocale)
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
import Data.Time.Format (parseTimeM)
import Data.Time.LocalTime (TimeZone, timeZoneOffsetString)
import Hours.Budget (Interval(..))

parseTimeClockEntry :: TimeZone -> String -> (Bool, UTCTime)
parseTimeClockEntry zone s = case words s of
    "i"                 : d : t : _ -> (True,  parseIso (d <> " " <> t))
    (map toLower -> "o") : d : t : _ -> (False, parseIso (d <> " " <> t))
    _ -> error $ "Invalid timeclock line: '" <> s <> "'"
  where
    parseIso t = fromMaybe
        (error ("Failed to parse time: " ++ t))
        (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%z"
             (t ++ timeZoneOffsetString zone))

parseLogbook :: TimeZone -> UTCTime -> String
             -> ([UTCTime], [Interval UTCTime NominalDiffTime])
parseLogbook zone now s = (st, sortOn begin ints')
  where
    entries = map (parseTimeClockEntry zone) (lines s)
    (ints, st) = runState (foldM go [] entries) []
    ints' = map (\i -> Interval i now (diffUTCTime now i)) st ++ ints
    go xs (isIn, t)
        | isIn = xs <$ modify (t:)
        | otherwise = do
              vs <- get
              case vs of
                  [] -> error "Nothing to clock out of"
                  i:is -> do
                      put is
                      return $ Interval i t (diffUTCTime t i) : xs
