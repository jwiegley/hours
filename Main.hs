{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main where

import           Budget (Interval(..))
import qualified Budget (sumRange, current, divideIntervals, activeIntervals)
import           Control.Exception (assert)
import           Control.Monad (ap)
import           Data.Char (toLower)
import           Data.Foldable (Foldable(foldl'))
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Data.Maybe (isJust, fromMaybe)
import           Data.Proxy (Proxy(..))
import           Data.Reflection (Reifies(..), reify)
import           Data.Semigroup (Semigroup((<>)))
import           Data.Tagged (Tagged(..))
import           Data.Time (defaultTimeLocale)
import           Data.Time.Calendar (Day, toGregorian, fromGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Data.Time.CalendarTime (CalendarTimeConvertible(..))
import           Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime, addUTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import           Data.Time.Format (parseTimeM, formatTime)
import           Data.Time.LocalTime
import           Data.Time.Moment (Moment(..))
import           Data.Time.Recurrence as R
import           Options.Applicative
import           System.Process (readProcessWithExitCode)
import           Text.Printf (printf)
-- import           Debug.Trace

timeZoneBAE :: TimeZone
timeZoneBAE = TimeZone (-300) False "EST"

holidayTable :: [ZonedTime]
holidayTable =
    [ mkZonedTime timeZoneBAE 2018 1 1 8 0
    ]

------------------------------------------------------------------------------
-- Additional support code for ZonedTime values

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

instance Reifies s TimeZone => CalendarTimeConvertible (Tagged s ZonedTime) where
  toCalendarTime (Tagged z) = toCalendarTime z
  fromCalendarTime t = Tagged <$> fromCalendarTime t

mkZonedTime :: TimeZone -> Integer -> Int -> Int -> Int -> Int -> ZonedTime
mkZonedTime tz y m d hh mm =
    ZonedTime (LocalTime (fromGregorian y m d) (TimeOfDay hh mm 0)) tz

diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime x y = diffUTCTime (zonedTimeToUTC x)
                                (zonedTimeToUTC y)

addZonedTime :: NominalDiffTime -> ZonedTime -> ZonedTime
addZonedTime x y = utcToZonedTime (zonedTimeZone y)
                                  (addUTCTime x (zonedTimeToUTC y))

dayOf :: ZonedTime -> Day
dayOf = localDay . zonedTimeToLocalTime

timeOf :: ZonedTime -> TimeOfDay
timeOf = localTimeOfDay . zonedTimeToLocalTime

toHours :: NominalDiffTime -> Double
toHours x = realToFrac x / 3600.0 :: Double

fromHours :: Int -> NominalDiffTime
fromHours x = fromIntegral (x * 3600)

------------------------------------------------------------------------------
-- Library code for this module

data WorkHours
    = Holiday
    | OffFriday
    | HalfFriday
    | RegularDay
    deriving (Eq, Show)

workHoursToInt :: WorkHours -> Int
workHoursToInt Holiday    = 0
workHoursToInt OffFriday  = 0
workHoursToInt HalfFriday = 4
workHoursToInt RegularDay = 9

data Variant
    = BoolVal Bool
    | DiffTimeVal NominalDiffTime
    | DoubleVal Double
    | IntVal Int
    | StringVal String
    | SymbolVal String
    | TimeVal ZonedTime
    deriving (Eq, Ord, Show)

variantToLisp :: Variant -> String
variantToLisp = \case
    BoolVal     True  -> "t"
    BoolVal     False -> "nil"
    DoubleVal   x     -> printf "%.1f" x
    DiffTimeVal x     -> printf "%.1f" (toHours x)
    IntVal      x     -> show x
    StringVal   x     -> show x
    SymbolVal   x     -> x
    TimeVal     x     ->
        let secs = floor (utcTimeToPOSIXSeconds
                          (zonedTimeToUTC x)) :: Int in
        printf "(%d %d 0 0)" (secs `div` 2^(16 :: Int))
                             (secs `mod` 2^(16 :: Int))

------------------------------------------------------------------------------
-- Generate the list of intervals for expected work

twoWeekStart :: ZonedTime
twoWeekStart = mkZonedTime timeZoneBAE 2017 12 29 8 0

twoWeekDates :: [ZonedTime]
twoWeekDates = reify timeZoneBAE $ \(Proxy :: Proxy s) ->
    fmap unTagged
        . starting (Tagged  twoWeekStart :: Tagged s ZonedTime)
        $ recur (daily `by` 14)

baeTwoWeekRange :: ZonedTime -> (ZonedTime, ZonedTime)
baeTwoWeekRange moment =
    interval (mkZonedTime timeZoneBAE year month day hour 0) twoWeekDates
  where
    (fromIntegral -> year, fromIntegral -> month, fromIntegral -> day) =
        toGregorian (dayOf moment)

    hour = todHour (timeOf moment)

    interval t (x:y:xs) | y > t     = (x, y)
                        | otherwise = interval t (y:xs)
    interval _ _ = error "impossible"

workIntervals :: ZonedTime -> ZonedTime -> [Interval ZonedTime WorkHours]
workIntervals beg end = reify (zonedTimeZone beg) $ \(Proxy :: Proxy s) ->
    reverse . foldl' go []
            . ap zip tail
            . takeWhile (<= end)
            . map unTagged
            . starting (Tagged beg :: Tagged s ZonedTime)
            $ recur daily >==> R.filter (WeekDays [Monday .. Friday])
  where
    go xs (b, e)
        | b == beg =
          Interval b (addZonedTime (fromIntegral fullday) b) OffFriday : xs

        | b `elem` holidayTable =
          Interval b e Holiday : xs

        | otherwise =
          let (_, _, w) = toWeekDate (dayOf b)
          in case w of
               5 -> let mid = addZonedTime (fromIntegral midshift) b in
                   Interval mid e HalfFriday :
                   Interval b mid HalfFriday : xs
               _ ->
                   Interval b e RegularDay : xs

    midshift = (3600 :: Int) * 4
    fullday  = (3600 :: Int) * 24

------------------------------------------------------------------------------
-- Parse the timelog of actually worked intervals

parseTimeClockEntry :: TimeZone
                    -> String
                    -> (Bool, Either ZonedTime (ZonedTime, String, String))
parseTimeClockEntry zone s = case words s of
    "i" : d : t : _ ->
        let account : (intercalate "  " -> payee) = splitOn "  " (drop 22 s)
        in (True, Right (parseIso (d <> " " <> t), account, payee))
    (map toLower -> "o") : d : t : _ ->
        (False, Left (parseIso (d <> " " <> t)))
    _ -> error $ "Invalid timeclock line: '" <> s <> "'"
  where
    parseIso t = fromMaybe
        (error $ "Failed to parse time: " ++ t)
        (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%z"
             (t ++ timeZoneOffsetString zone))

parseLogbook :: ZonedTime
             -> String
             -> (Bool, [Interval ZonedTime (String, String)])
parseLogbook now s = (isJust st, reverse ints')
  where
    (st, ints) = foldl' go (Nothing, []) (parseEntries (lines s))

    parseEntries = map (parseTimeClockEntry (zonedTimeZone now))

    ints' = case st of
        Nothing -> ints
        Just (i, a, p) -> Interval i now (a, p) : ints

    go (mbeg, xs) (isIn, x) = case (isIn, mbeg, x) of
        (True, Just _, _)        -> error "Already clocked in"
        (False, Nothing, _)      -> error "Nothing to clock out of"
        (True, Nothing, Left _)  -> error "Clock in without details"
        (False, Just _, Right _) -> error "Clock out with details"
        (True, Nothing, Right v) -> (Just v, xs)
        (False, Just (i, a, p), Left o) ->
            (Nothing, Interval i o (a, p) : xs)

------------------------------------------------------------------------------
-- Budget calculations

data Budget = Budget
    { budgetStart               :: ZonedTime
    , budgetNow                 :: ZonedTime
    , budgetEnd                 :: ZonedTime

    , budgetIdealExpected       :: Int
    , budgetIdealRemaining      :: Int
    , budgetIdealExpectedExact  :: NominalDiffTime
    , budgetIdealRemainingExact :: NominalDiffTime
    , budgetIdealTotal          :: Int
    , budgetIdealDaysLeft       :: Int
    , budgetIdealDaysLeftIncl   :: Int
    , budgetIdealPaceMark       :: Double

    , budgetRealCompleted       :: NominalDiffTime
    , budgetRealRemaining       :: NominalDiffTime
    , budgetRealThisExpected    :: NominalDiffTime
    , budgetRealThisCompleted   :: NominalDiffTime
    , budgetRealThisRemaining   :: NominalDiffTime
    , budgetRealDiscrepancy     :: NominalDiffTime
    , budgetRealPaceMark        :: Double

    , budgetLoggedIn            :: Bool
    , budgetThisSym             :: String
    }

instance Show Budget where
  show Budget {..} = let v = variantToLisp in concat
    [ "((beg . ",                  v (TimeVal budgetStart), ")\n"
    , "(now . ",                   v (TimeVal budgetNow), ")\n"
    , "(end . ",                   v (TimeVal budgetEnd), ")\n"
    , "(ideal-expected . ",        v (IntVal budgetIdealExpected), ")\n"
    , "(ideal-remaining . ",       v (IntVal budgetIdealRemaining), ")\n"
    , "(ideal-expected-exact . ",  v (DiffTimeVal budgetIdealExpectedExact), ")\n"
    , "(ideal-remaining-exact . ", v (DiffTimeVal budgetIdealRemainingExact), ")\n"
    , "(ideal-total . ",           v (IntVal budgetIdealTotal), ")\n"
    , "(ideal-days-left . ",       v (IntVal budgetIdealDaysLeft), ")\n"
    , "(ideal-days-left-incl . ",  v (IntVal budgetIdealDaysLeftIncl), ")\n"
    , "(ideal-pace-mark . ",       v (DoubleVal budgetIdealPaceMark), ")\n"
    , "(real-completed . ",        v (DiffTimeVal budgetRealCompleted), ")\n"
    , "(real-remaining . ",        v (DiffTimeVal budgetRealRemaining), ")\n"
    , "(real-this-expected . ",    v (DiffTimeVal budgetRealThisExpected), ")\n"
    , "(real-this-completed . ",   v (DiffTimeVal budgetRealThisCompleted), ")\n"
    , "(real-this-remaining . ",   v (DiffTimeVal budgetRealThisRemaining), ")\n"
    , "(real-discrepancy . ",      v (DiffTimeVal budgetRealDiscrepancy), ")\n"
    , "(real-pace-mark . ",        v (DoubleVal budgetRealPaceMark), ")\n"
    , "(logged-in . ",             v (BoolVal budgetLoggedIn), ")\n"
    , "(this-sym . ",              v (SymbolVal budgetThisSym), "))\n"
    ]

calculateBudget :: ZonedTime -> String -> Budget
calculateBudget now activeTimelog =
    -- trace ("workints = " ++ Budget.showIntervals workints) $
    -- trace ("active   = " ++ Budget.showIntervals active)   $
    -- trace ("active'  = " ++ Budget.showIntervals active')  $
    -- trace ("future   = " ++ Budget.showIntervals future)   $
    -- trace ("future'  = " ++ Budget.showIntervals future')  $
    -- trace ("logbook  = " ++ Budget.showIntervals logbook)  $
    -- trace ("tdys     = " ++ Budget.showIntervals tdys)     $

    assert (beg <= now) $
    assert (now < end)

    Budget { budgetStart               = beg
           , budgetNow                 = now
           , budgetEnd                 = end

           , budgetIdealExpected       = expected
           , budgetIdealRemaining      = remaining
           , budgetIdealExpectedExact  = expected'
           , budgetIdealRemainingExact = remaining'
           , budgetIdealTotal          = totalWork
           , budgetIdealDaysLeft       = length future
           , budgetIdealDaysLeftIncl   = length future'
           , budgetIdealPaceMark       = passage

           , budgetRealCompleted       = completed
           , budgetRealRemaining       = hoursLeft
           , budgetRealThisExpected    = thisExp
           , budgetRealThisCompleted   = thisDone
           , budgetRealThisRemaining   = thisLeft
           , budgetRealDiscrepancy     = discrep
           , budgetRealPaceMark        = paceMark

           , budgetLoggedIn            = loggedIn
           , budgetThisSym             = thisSym
           }
  where
    (fromIntegral -> yr, fromIntegral -> mon, fromIntegral -> day) =
        toGregorian (dayOf now)

    (beg, end) = baeTwoWeekRange now
    workints = workIntervals beg end

    (loggedIn, logbook) = parseLogbook now activeTimelog

    (active, future) = Budget.activeIntervals now workints
    (active', future') =
        Budget.divideIntervals diffZonedTime (*) now
            (map (fmap (fromHours . workHoursToInt)) workints)

    sumWork    = Budget.sumRange . map (fmap workHoursToInt)
    expected   = sumWork active
    remaining  = sumWork future
    totalWork  = expected + remaining
    expected'  = Budget.sumRange active'
    remaining' = Budget.sumRange future'
    passage    = (100.0 * toHours expected') / fromIntegral totalWork

    timesVal i = i { intVal = diffZonedTime (intEnd i) (intBeg i) }
    worked     = map timesVal logbook
    completed  = Budget.sumRange worked
    hoursLeft  = fromHours totalWork - completed
    discrep    = completed - expected'
    paceMark   = (100.0 * toHours completed) / fromIntegral totalWork

    thisBeg    = mkZonedTime (zonedTimeZone now) yr mon day 0 0
    (_, tdys)  = Budget.divideIntervals diffZonedTime (*) thisBeg worked
    thisDone   = Budget.sumRange tdys
    thisExp    = (hoursLeft + thisDone) / fromIntegral (length future')
    thisLeft   = thisExp - thisDone

    thisSym = case Budget.current now workints of
        Just (intVal -> Holiday)    -> "holiday"
        Just (intVal -> OffFriday)  -> "off-friday"
        Just (intVal -> HalfFriday) -> "half-friday"
        Just (intVal -> RegularDay) -> "regular-day"
        _                          -> "not-working"

doMain :: Options -> IO ()
doMain opts = do
    now <- utcToZonedTime <$> getCurrentTimeZone <*> getCurrentTime

    let (beg, end) = baeTwoWeekRange now
        fmtTime    = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
        begs       = fmtTime beg
        ends       = fmtTime end

    (_, activeTimelog, _) <-
        readProcessWithExitCode "org2tc" [file opts, "-s", begs, "-e", ends] ""

    let budget = calculateBudget now activeTimelog

    if emacs opts
        then print budget
        else printf "%s%s%.1fh %.0f%% (%.1fh)\n"
            (if budgetLoggedIn budget
             then printf "\ESC[37m🕓\ESC[0m%.1fh "
                (toHours (budgetRealThisCompleted budget))
             else "")
            (if budgetRealDiscrepancy budget < 0
                then "\ESC[0;31m↓\ESC[0m"
                else "\ESC[0;32m↑\ESC[0m")
            (abs (toHours (budgetRealDiscrepancy budget)))
            (budgetRealPaceMark budget)
            (toHours (budgetRealRemaining budget))

------------------------------------------------------------------------------
-- Main driver

data Options = Options
    { file  :: String
    , emacs :: Bool
    }

options :: Parser Options
options = Options
    <$> strOption (long "file" <> help "Active timelog file to use")
    <*> switch (long "emacs" <> help "Emit statistics in Emacs Lisp form")

main :: IO ()
main = execParser opts >>= doMain
  where
    opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "Show hours worked so far"
                 <> header "hours - show hours worked so far")

-- Main.hs (hours) ends here
