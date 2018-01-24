{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Budget
import           Budget (Interval(..))
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Data.Proxy
import           Data.Reflection
import           Data.Semigroup
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (defaultTimeLocale)
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.CalendarTime
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Time.Moment hiding (Interval, interval)
import           Data.Time.Recurrence as R
import           Options.Applicative
import           Shelly
import           Text.Printf

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

timeZoneEST :: TimeZone
timeZoneEST = TimeZone (-300) False "EST"

holidayTable :: [(Integer, [(Int, [Int])])]
holidayTable = [ (2018, [ ( 1, [1]) ])
               ]

data Variant
    = BoolVal Bool
    | DiffTimeVal NominalDiffTime
    | IntVal Int
    | DateTripleVal (Integer, Int, Int)
    | MaybeTextVal (Maybe Text)
    | StringVal String
    | TimeVal ZonedTime
    deriving (Eq, Ord, Show)

data WorkHours
    = NoWork
    | OffFriday
    | HalfFriday
    | RegularDay
    deriving (Eq, Show)

workHoursToInt :: WorkHours -> Int
workHoursToInt NoWork = 0
workHoursToInt OffFriday = 0
workHoursToInt HalfFriday = 4
workHoursToInt RegularDay = 9

midshift :: Int
midshift = 3600 * 4

fullday :: Int
fullday = 3600 * 24

twoWeekStart :: ZonedTime
twoWeekStart = mkZonedTime timeZoneEST 2017 12 29 8 0

twoWeekDates :: [ZonedTime]
twoWeekDates = reify timeZoneEST $ \(Proxy :: Proxy s) ->
    fmap unTagged
        . starting (Tagged  twoWeekStart :: Tagged s ZonedTime)
        $ recur (daily `by` 14)

baeTwoWeekRange :: Integer -> Int -> Int -> Int -> (ZonedTime, ZonedTime)
baeTwoWeekRange year month day hour =
    interval (mkZonedTime timeZoneEST year month day hour 0) twoWeekDates
  where
    interval t (x:y:xs) | y > t     = (x, y)
                        | otherwise = interval t (y:xs)
    interval _ _ = error "impossible"

holidays :: Integer -> Int -> [Int]
holidays year month =
    fromMaybe [] $ join $ lookup month <$> lookup year holidayTable

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
        | otherwise =
          let (_, _, w) = toWeekDate (dayOf b)
          in case w of
               5 -> let mid = addZonedTime (fromIntegral midshift) b in
                   Interval mid e HalfFriday :
                   Interval b mid HalfFriday : xs
               _ ->
                   Interval b e RegularDay : xs

parseTimeClockEntry :: Text
                    -> (Bool, Either ZonedTime (ZonedTime, Text, Text))
parseTimeClockEntry s = case T.words s of
    "i" : d : t : _ ->
        let (account, T.drop 2 -> payee) = T.breakOn " " (T.drop 22 s)
        in (True, Right (parseIso (d <> " " <> t), account, payee))
    (T.toLower -> "o") : d : t : _ -> (False, Left (parseIso (d <> " " <> t)))
    _ -> error . T.unpack $ "Invalid timeclock line: '" <> s <> "'"
  where
    parseIso t = fromMaybe (error $ "Failed to parse time: " ++ T.unpack t)
        (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t))

parseLogbook :: ZonedTime
             -> Text
             -> (Bool, [Interval ZonedTime (Text, Text)])
parseLogbook now s = (isJust st, reverse ints')
  where
    (st, ints) =
        foldl' go (Nothing, []) . map parseTimeClockEntry . T.lines $ s

    ints' = case st of
        Nothing -> ints
        Just (!i, !a, !p) -> Interval i now (a, p) : ints

    go (!mbeg, !xs) (!isIn, !x) = case (isIn, mbeg, x) of
        (True, Just _, _)        -> error "Already clocked in"
        (False, Nothing, _)      -> error "Nothing to clock out of"
        (True, Nothing, Left _)  -> error "Clock in without details"
        (False, Just _, Right _) -> error "Clock out with details"
        (True, Nothing, Right v) -> (Just v, xs)
        (False, Just (!i, !a, !p), Left o) ->
            (Nothing, Interval i o (a, p) : xs)

data Options = Options
    { verbose  :: Bool
    , file     :: String
    , category :: String
    , archive  :: String
    , emacs    :: Bool
    }

options :: Parser Options
options = Options
    <$> switch (long "verbose" <> help "Display statistics")
    <*> strOption (long "file" <> help "Active timelog file to use")
    <*> strOption (long "category"
                   <> help "Account/category to query from timelog"
                   <> value "")
    <*> strOption (long "archive" <> help "Archival timelog" <> value "")
    <*> switch (long "emacs" <> help "Emit statistics in Emacs Lisp form")

main :: IO ()
main = execParser opts >>= doMain
  where
    opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "Show hours worked so far"
                 <> header "hours - show hours worked so far")

doMain :: Options -> IO ()
doMain opts = do
    now <- utcToZonedTime <$> getCurrentTimeZone <*> getCurrentTime

    let today      = toGregorian (dayOf now)
        yr         = fromIntegral (today^._1)
        mon        = fromIntegral (today^._2)
        day        = fromIntegral (today^._3)
        (beg, end) = baeTwoWeekRange yr mon day (todHour (timeOf now))
        fmtTime    = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
        begs       = fmtTime beg
        ends       = fmtTime end

    activeTimelog <- shelly $ silently $
        run "org2tc" [T.pack (file opts), "-s", begs, "-e", ends]

    let (loggedIn, logbook) = parseLogbook now activeTimelog
        workints    = workIntervals beg end
        toHours x   = realToFrac x / 3600.0 :: Double
        fromHours x = fromIntegral (x * 3600)
        (active, future) = Budget.activeIntervals now workints
        (active', future') =
            Budget.divideIntervals diffZonedTime (*) now
                (map (fmap (fromHours . workHoursToInt)) workints)
        sumWork     = Budget.sumRange . map (fmap workHoursToInt)
        expected    = sumWork active
        expected'   = Budget.sumRange active'
        remaining   = sumWork future
        remaining'  = Budget.sumRange future'
        workedT     =
            map (\i -> i { intVal = diffZonedTime (intEnd i) (intBeg i) }) logbook
        sumWorked   = sum . map intVal
        completed   = sumWorked workedT
        hoursLeft   = fromHours (expected + remaining) - completed
        mkTime      = mkZonedTime (zonedTimeZone now) yr mon day
        todayBeg    = mkTime 0 0
        (_, tdys)   = Budget.divideIntervals diffZonedTime (*) todayBeg workedT
        doneToday   = sumWorked tdys
        discrep     = completed - fromHours expected
        paceMark    = remaining' - hoursLeft
        futureDays  = hoursLeft / fromIntegral (length future)
        indicator   = if discrep < 0
                      then "\ESC[0;31m↓\ESC[0m"
                      else "\ESC[0;32m↑\ESC[0m" :: Text

        details :: [(Text, Variant)]
        details = [ ("beg",              TimeVal beg)
                  , ("now",              TimeVal now)
                  , ("end",              TimeVal end)
                  , ("work-expected",    IntVal expected)
                  , ("work-expected_",   DiffTimeVal expected')
                  , ("work-remaining",   IntVal remaining)
                  , ("work-remaining_",  DiffTimeVal remaining')
                  , ("work-total",       IntVal (expected + remaining))
                  , ("work-days-left",   IntVal (length future))
                  , ("my-today-done",    DiffTimeVal doneToday)
                  , ("my-today-left",    DiffTimeVal (futureDays - doneToday))
                  , ("my-upcoming-days", DiffTimeVal futureDays)
                  , ("my-completed",     DiffTimeVal completed)
                  , ("my-remaining",     DiffTimeVal hoursLeft)
                  , ("my-discrepancy",   DiffTimeVal discrep)
                  , ("pace-mark",        DiffTimeVal paceMark)
                  , ("logged-in",        BoolVal loggedIn)
                  ]

    when (verbose opts) $ do
        putStrLn $ "workints = " ++ Budget.showIntervals workints
        putStrLn $ "active   = " ++ Budget.showIntervals active
        putStrLn $ "active'  = " ++ Budget.showIntervals active'
        putStrLn $ "future   = " ++ Budget.showIntervals future
        putStrLn $ "future'  = " ++ Budget.showIntervals future'

        forM_ details $ \(n, v) -> printf "%s: %s\n" n (show v)

    if emacs opts
        then do
            putStr "("
            forM_ details $ \(n, v) -> do
                putStr "("
                putStr (T.unpack n)
                putStr " . "
                putStr $ case v of
                    BoolVal       True      -> "t"
                    BoolVal       False     -> "nil"
                    DiffTimeVal   x         -> printf "%.1f" (toHours x)
                    IntVal        x         -> show x
                    DateTripleVal (y, m, d) -> printf "(%d %d %d)" y m d
                    MaybeTextVal  (Just x)  -> show x
                    MaybeTextVal  Nothing   -> "nil"
                    StringVal     x         -> show x
                    TimeVal       x         ->
                        let secs = floor (utcTimeToPOSIXSeconds
                                          (zonedTimeToUTC x)) :: Int in
                        printf "(%d %d 0 0)"
                            (secs `div` 2^(16 :: Int))
                            (secs `mod` 2^(16 :: Int))
                putStrLn ")"
            putStrLn ")"

        else printf "%s%s%.1fh %.0f%% (%.1fh)\n"
            (if loggedIn
             then printf "\ESC[37m🕓\ESC[0m%.1fh " (toHours doneToday)
             else T.unpack "")
            indicator (abs (toHours discrep)) (toHours paceMark)
            (toHours hoursLeft)

-- Main.hs (hours) ends here
