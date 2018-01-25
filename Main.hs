{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main where

import qualified Budget
import           Budget (Interval(..))
import           Data.Char (toLower)
import           Data.Foldable (Foldable(foldl'))
import           Data.List (intercalate, sortOn)
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
import           Data.Time.Clock (NominalDiffTime, getCurrentTime,
                                  diffUTCTime, addUTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds,
                                        posixSecondsToUTCTime)
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
    [ mkZonedTime timeZoneBAE 2018 1 1 9 0
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

------------------------------------------------------------------------------
-- Library code for this module

data WorkHours
    = Holiday
    | OffFriday
    | HalfFriday
    | RegularDay
    deriving (Eq, Show)

workHoursToInt :: Bool -> WorkHours -> Int
workHoursToInt _     Holiday    = 0
workHoursToInt _     OffFriday  = 0
workHoursToInt _     HalfFriday = 4
workHoursToInt False RegularDay = 9
workHoursToInt True  RegularDay = 8

data Variant
    = BoolVal Bool
    | DiffTimeVal NominalDiffTime
    | DoubleVal Double
    | IntVal Int
    | StringVal String
    | WorkHoursVal (Maybe WorkHours)
    | TimeVal ZonedTime
    deriving (Eq, Show)

variantToLisp :: Variant -> String
variantToLisp = \case
    BoolVal      True  -> "t"
    BoolVal      False -> "nil"
    DoubleVal    x     -> printf "%.1f" x
    DiffTimeVal  x     -> printf "%.1f" (toHours x)
    IntVal       x     -> show x
    StringVal    x     -> show x
    WorkHoursVal x     -> case x of
        Just Holiday    -> "holiday"
        Just OffFriday  -> "off-friday"
        Just HalfFriday -> "half-friday"
        Just RegularDay -> "regular-day"
        Nothing         -> "not-working"
    TimeVal      x     ->
        let secs = floor (utcTimeToPOSIXSeconds
                          (zonedTimeToUTC x)) :: Int in
        printf "(%d %d 0 0)" (secs `div` 2^(16 :: Int))
                             (secs `mod` 2^(16 :: Int))

------------------------------------------------------------------------------
-- Generate the list of intervals for expected work

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
workIntervals mine beg end = reify (zonedTimeZone beg) $ \(Proxy :: Proxy s) ->
    concatMap go
        . takeWhile (< end)
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

------------------------------------------------------------------------------
-- Parse the timelog of actually worked intervals

parseTimeClockEntry :: TimeZone -> String
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

parseLogbook :: ZonedTime -> String
             -> (Bool, [Interval ZonedTime (String, String)])
parseLogbook now s = (isJust st, sortOn Budget.begin ints')
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
    { bStart                 :: ZonedTime
    , bNow                   :: ZonedTime
    , bEnd                   :: ZonedTime
    , bIdealExpected         :: Int
    , bIdealRemaining        :: Int
    , bIdealExpectedExact    :: NominalDiffTime
    , bIdealRemainingExact   :: NominalDiffTime
    , bIdealDaysLeft         :: Int
    , bIdealDaysLeftIncl     :: Int
    , bRealCompleted         :: NominalDiffTime
    , bRealExpected          :: NominalDiffTime
    , bRealExpectedInact     :: NominalDiffTime
    , bRealThisCompleted     :: NominalDiffTime
    , bRealThisRemaining     :: NominalDiffTime
    , bLoggedIn              :: Bool
    , bThisSym               :: Maybe WorkHours
    }

instance Show Budget where
  show Budget {..} = let v = variantToLisp in concat
    [ "((beg . ",                  v (TimeVal bStart), ")\n"
    , "(now . ",                   v (TimeVal bNow), ")\n"
    , "(end . ",                   v (TimeVal bEnd), ")\n"
    , "(ideal-total . ",           v (IntVal idealTotal), ")\n"
    , "(ideal-expected . ",        v (IntVal bIdealExpected), ")\n"
    , "(ideal-remaining . ",       v (IntVal bIdealRemaining), ")\n"
    , "(ideal-expected-exact . ",  v (DiffTimeVal bIdealExpectedExact), ")\n"
    , "(ideal-remaining-exact . ", v (DiffTimeVal bIdealRemainingExact), ")\n"
    , "(ideal-days-left . ",       v (IntVal bIdealDaysLeft), ")\n"
    , "(ideal-days-left-incl . ",  v (IntVal bIdealDaysLeftIncl), ")\n"
    , "(real-completed . ",        v (DiffTimeVal bRealCompleted), ")\n"
    , "(real-expected . ",         v (DiffTimeVal bRealExpected), ")\n"
    , "(real-expected-inact . ",   v (DiffTimeVal bRealExpectedInact), ")\n"
    , "(real-this-completed . ",   v (DiffTimeVal bRealThisCompleted), ")\n"
    , "(real-this-remaining . ",   v (DiffTimeVal bRealThisRemaining), ")\n"
    , "(real-discrepancy . ",      v (DiffTimeVal discrepancy), ")\n"
    , "(logged-in . ",             v (BoolVal bLoggedIn), ")\n"
    , "(this-sym . ",              v (WorkHoursVal bThisSym), "))\n"
    ]
   where
     discrepancy = bRealCompleted - bIdealExpectedExact
     idealTotal  = bIdealExpected + bIdealRemaining

calculateBudget :: ZonedTime -> String -> Budget
calculateBudget now activeTimelog =
    -- trace ("bStart   = " ++ show bStart) $
    -- trace ("now      = " ++ show now) $
    -- trace ("bNow     = " ++ show bNow) $
    -- trace ("now'     = " ++ show now') $
    -- trace ("nowBAE   = " ++ show nowBAE) $
    -- trace ("bEnd     = " ++ show bEnd) $
    -- trace ("workints = " ++ Budget.showIntervals workIntsWorkHours) $
    -- trace ("active   = " ++ Budget.showIntervals active) $
    -- trace ("active'  = " ++ Budget.showIntervals active') $
    -- trace ("future   = " ++ Budget.showIntervals future) $
    -- trace ("future'  = " ++ Budget.showIntervals future') $
    -- trace ("logbook  = " ++ Budget.showIntervals logbook) $
    -- trace ("today    = " ++ Budget.showIntervals today) $

    Budget {..}
  where
    bNow                  = now
    (bStart, bEnd)        = baeTwoWeekRange nowBAE
    bIdealExpected        = Budget.sumValues active
    bIdealRemaining       = Budget.sumValues future
    bIdealRemainingExact  = Budget.sumValues future'
    bIdealExpectedExact   = Budget.sumValues active'
    bIdealDaysLeft        = length future
    bIdealDaysLeftIncl    = length future' + maybe 1 (const 0) current
    bRealCompleted        = Budget.sumValues logHours
    bRealExpectedInact    = hoursLeft / fromIntegral (length future)
    bRealExpected         = if bLoggedIn
                            then activeExp
                            else bRealExpectedInact

    bRealThisCompleted    = Budget.sumValues today
    bRealThisRemaining    = activeExp - bRealThisCompleted
    bThisSym              = Budget.value <$> current

    useMyNotionOfWorkTime = True -- Instead of BAE's work schedule using
                                 -- off-Fridays, I use a normal work pattern
                                 -- based on not taking off those Fridays.

    -- Since I don't work 6-2 PST, I adjust real expectations to compute as if
    -- I were situated at the BAE office and working from there. This better
    -- models an ordinary 9-5 workday.
    secsAway              = diffTimeZone timeZoneBAE (zonedTimeZone bNow)
    now'                  = addZonedTime (- secsAway) bNow
    nowBAE                = setTimeZone timeZoneBAE now'
    hoursToInt            = workHoursToInt useMyNotionOfWorkTime
    workIntsWorkHours     = workIntervals useMyNotionOfWorkTime bStart bEnd
    workIntsHours         = Budget.mapValues hoursToInt workIntsWorkHours
    workIntsDiffTime      = Budget.mapValues fromHours workIntsHours
    (active, future)      = Budget.activeIntervals nowBAE workIntsHours
    current               = Budget.current nowBAE workIntsWorkHours
    totalWork             = bIdealExpected + bIdealRemaining
    (active', future')    = Budget.divideIntervals nowBAE workIntsDiffTime
    (bLoggedIn, logbook)  = parseLogbook bNow activeTimelog
    logHours              = map Budget.boundsToValue logbook
    hoursLeft             = fromHours totalWork - bRealCompleted
    thisBeg               = setHour 0 bNow
    (_, today)            = Budget.divideIntervals thisBeg logHours
    activeExp             = (hoursLeft + bRealThisCompleted)
                                / fromIntegral bIdealDaysLeftIncl

doMain :: Options -> IO ()
doMain opts = do
    now <- utcToZonedTime <$> getCurrentTimeZone <*> getCurrentTime

    let (beg, end) = baeTwoWeekRange now
        fmtTime    = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

    (_, activeTimelog, _) <-
        readProcessWithExitCode "org2tc"
            [file opts, "-s", fmtTime beg, "-e", fmtTime end] ""

    let b = calculateBudget now activeTimelog
    if emacs opts
        then print b
        else do
            let discrepancy = bRealCompleted b - bIdealExpectedExact b
                idealTotal  = bIdealExpected b + bIdealRemaining b
            printf "%s%s%.1fh %.0f%% (%.1fh)\n"
                (if bLoggedIn b
                 then printf "\ESC[37m🕓\ESC[0m%.1fh "
                    (toHours (bRealThisCompleted b))
                 else "")
                (if discrepancy < 0
                 then "\ESC[0;31m↓\ESC[0m"
                 else "\ESC[0;32m↑\ESC[0m")
                (abs (toHours discrepancy))
                ((100.0 * toHours (bRealCompleted b))
                    / fromIntegral idealTotal)
                (toHours (fromHours idealTotal - bRealCompleted b))

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
