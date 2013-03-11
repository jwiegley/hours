{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as B
import           Data.List
import           Data.Maybe
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import qualified Data.Time.Parsers as Time
import           Data.Time.Recurrence as R
import           Options.Applicative
import           Prelude hiding (filter)
import           Shelly
import           System.Environment
import           System.IO.Unsafe
import           System.Locale
import           Text.Printf

default (Integer, Text)

mkUTCTime :: (Integral a, Real b) => a -> a -> a -> a -> a -> b -> UTCTime
mkUTCTime year month day hour minute second =
  localTimeToUTC (hoursToTimeZone 0)
    (LocalTime
      (fromGregorian (fromIntegral year) (fromIntegral month)
                     (fromIntegral day))
      (TimeOfDay (fromIntegral hour) (fromIntegral minute) (realToFrac second)))

monthRange :: Integral a => a -> a -> (UTCTime,UTCTime)
monthRange year month =
  (mkUTCTime year month 1 0 0 0,
   mkUTCTime (if month == 12 then year + 1 else year)
             (if month == 12 then 1 else month + 1) 1 0 0 0)

federalHolidays :: Int -> Int -> Int
federalHolidays year month =
    fromMaybe 0 $ join $ lookup month <$> lookup year
        [ (2013, [(1,2),(2,1),(5,1),(7,1),(9,1),(10,1),(11,2),(12,1)])
        , (2014, [(1,2),(2,1),(5,1),(7,1),(9,1),(10,1),(11,2),(12,1)])
        , (2015, [(1,2),(2,1),(5,1),(7,1),(9,1),(10,1),(11,2),(12,1)])
        , (2016, [(1,2),(2,1),(5,1),(7,1),(9,1),(10,1),(11,2),(12,1)])
        , (2017, [(1,2),(2,1),(5,1),(7,1),(9,1),(10,1),(11,2),(12,1)])
        , (2018, [(1,2),(2,1),(5,1),(7,1),(9,1),(10,1),(11,2),(12,1)])
        , (2019, [(1,2),(2,1),(5,1),(7,1),(9,1),(10,1),(11,2),(12,1)])
        , (2020, [(1,2),(2,1),(5,1),(7,1),(9,1),(10,1),(11,2),(12,1)]) ]

countWorkDays :: Int -> Int -> UTCTime -> UTCTime -> Int
countWorkDays year month beg end =
    let holidays = federalHolidays year month
        days     = length . takeWhile (< end) . starting beg
                   $ recur daily >==> R.filter (WeekDays [Monday .. Friday])
    in days - holidays

isWeekendDay :: Day -> Bool
isWeekendDay day = let (_,_,dow) = toWeekDate day
                   in dow == 6 || dow == 7

balanceTotal :: Text -> Text -> Sh Float
balanceTotal journal period = do
    setStdin journal
    balance <- run "ledger" ["-f", "-", "--base", "-F", "%(scrub(total))\n"
                           , "-p", period, "--day-break", "bal"]
    return $ case T.lines balance of
        [] -> 0.0 :: Float
        xs -> (/ 3600.0)
              $ (read :: String -> Float) . T.unpack . T.init
              $ T.dropWhile (== ' ') (last xs)

data Options = Options
    { verbose  :: Bool
    , file     :: String
    , period   :: String
    , category :: String
    , archive  :: String
    , gratis   :: Int
    , moment   :: LocalTime
    }

options :: Parser Options
options = Options
    <$> switch (long "verbose" <> help "Display statistics")
    <*> strOption (long "file" <> help "Active timelog file to use")
    <*> strOption (long "period" <> help "Period to report for" <> value "")

    <*> strOption (long "category"
                   <> help "Account/category to query from timelog"
                   <> value "")

    <*> strOption (long "archive" <> help "Archival timelog" <> value "")
    <*> option (long "gratis" <> help "Hours given free each month" <> value 0)

    <*> option (long "moment" <> help "Set notion of the current moment"
                <> value (unsafePerformIO $
                          (zonedTimeToLocalTime <$>) getZonedTime)
                <> reader (Right . flip LocalTime midday . fromJust .
                           Atto.maybeResult .
                           Time.parseWithDefaultOptions Time.defaultDay .
                           B.pack))

main :: IO ()
main = execParser opts >>= doMain
  where
    opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "Show hours worked so far"
                 <> header "hours - show hours worked so far")

doMain :: Options -> IO ()
doMain opts = shelly $ silently $ do
    let per = if null (period opts)
               then Nothing
               else Just (T.pack (period opts))

    now <- if isNothing per
          then return (moment opts)
          else do
          dateString <-
              run "ledger" [ "eval", "--now", fromJust per, "today" ]
          return . fromJust $
              parseTime defaultTimeLocale "%Y/%m/%d" (T.unpack dateString)

    activeTimelog <- run "org2tc" [T.pack (file opts)]
    let (is, os) = partition (== 'i') $ map T.head (T.lines activeTimelog)
        loggedIn = length is > length os

    setStdin activeTimelog
    data1 <- run "ledger" (["-f", "-", "--day-break", "print"] <>
                          [T.pack (category opts)
                          | not (null (category opts))])
    data2 <- if null (archive opts)
            then return ""
            else run "org2tc" [T.pack (archive opts)] -|-
                 run "ledger" ["-f", "-", "--day-break", "print"]

    let combined = T.append data1 data2
    realHrs  <- balanceTotal combined (fromMaybe "this month" per)
    todayHrs <- balanceTotal combined "today"

    let today     = toGregorian (localDay now)
        currHour  = fromIntegral (todHour (localTimeOfDay now)) / 3.0 :: Float
        (yr,mon)  = (fromIntegral (today^._1), fromIntegral (today^._2))
        (beg,end) = monthRange yr mon
        workDays  = countWorkDays yr mon beg end
        gworkDays = workDays - gratis opts
        workHrs   = gworkDays * 8
        mnight    = localTimeToUTC (hoursToTimeZone 0)
                                   (LocalTime (localDay now) midnight)
        targDays  = countWorkDays yr mon beg mnight
        gtargDays = targDays - gratis opts
        targHrs   = gtargDays * 8
        isWeekend = isWeekendDay (localDay now)
        targetHrs = if isNothing per
                    then (if isWeekend then 0 else currHour) +
                         fromIntegral targHrs
                    else fromIntegral workHrs
        discrep   = realHrs - targetHrs
        hoursLeft = fromIntegral workHrs - realHrs
        indicator = if discrep < 0
                    then "\ESC[31m↓\ESC[0m"
                    else "\ESC[32m↑\ESC[0m"
        paceMark  = (realHrs * 100.0) / fromIntegral workHrs

    when (verbose opts) $ liftIO $ do
        putStrLn $ unlines
            [ "now:         " ++ show now
            , "today:       " ++ show today
            , "currHour:    " ++ show currHour
            , "todayHrs:    " ++ show todayHrs
            , "isWeekend:   " ++ show isWeekend
            , ""
            , "period:      " ++ show per
            , "beg:         " ++ show beg
            , "end:         " ++ show end
            , "days:        " ++ show (floor $ diffUTCTime end beg / 3600 / 24)
            , "workDays:    " ++ show workDays
            , "gworkDays:   " ++ show gworkDays
            , "workHrs:     " ++ show workHrs
            , "midnight:    " ++ show mnight
            , "targDays:    " ++ show targDays
            , "gtargDays:   " ++ show gtargDays
            , "targetHrs:   " ++ show targetHrs
            , ""
            , "realHrs:     " ++ show realHrs
            , "discrep:     " ++ show discrep
            , "hoursLeft:   " ++ show hoursLeft
            , "indicator:   " ++ show indicator
            , "paceMark:    " ++ show paceMark
            , ""
            , "length is:   " ++ show (length is)
            , "length os:   " ++ show (length os)
            , "loggedIn:    " ++ show loggedIn
            ]

    liftIO $ printf "%.1f%% %s%.1fh (%.1fh)%s\n"
        paceMark (T.unpack indicator) (abs discrep) hoursLeft
        (if loggedIn
         then printf "\n\ESC[37m⏱\ESC[0m %.2fh" todayHrs
         else T.unpack "")
  where
    getDays yr mon beg end  = (countWorkDays yr mon beg end - 1)
    getHours yr mon beg end = getDays yr mon beg end * 8

-- Main.hs (hours) ends here
