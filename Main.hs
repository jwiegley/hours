{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens hiding ( value )
import           Data.List
import           Data.Maybe
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Time.Recurrence as R
import           Prelude hiding (filter)
import           Shelly
import           System.Environment
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

countWorkDays :: UTCTime -> UTCTime -> Int
countWorkDays beg end =
    length . takeWhile (<= end) . starting beg $
        recur daily >==> R.filter (WeekDays [Monday .. Friday])

isWeekendDay :: Day -> Bool
isWeekendDay day = let (_,_,dow) = toWeekDate day
                   in dow == 6 || dow == 7

balanceTotal :: [String] -> Text -> Text -> Sh Float
balanceTotal args journal period = do
    setStdin journal
    balance <- run "ledger" ["-f", "-", "--base", "-F", "%(scrub(total))\n"
                           , "-p", if null args
                                   then period
                                   else T.pack (head args), "bal"]
    return $
        case T.lines balance of
            [] -> 0.0 :: Float
            xs -> (/ 3600.0)
                  $ (read :: String -> Float) . T.unpack . T.init
                  $ T.dropWhile (== ' ') (last xs)

debug :: Bool
debug = False

main :: IO ()
main = shelly $ silently $ do
    args <- liftIO $ getArgs
    now <- if null args
          then liftIO $ zonedTimeToLocalTime <$> getZonedTime
          else do
              dateString <-
                  run "ledger" ["eval", "--now", T.pack (head args), "today"]
              return . fromJust $
                  parseTime defaultTimeLocale "%Y/%m/%d" (T.unpack dateString)

    activeTimelog <- run "org2tc" ["/Users/johnw/doc/Tasks/todo.txt"]
    let (is, os) = partition (== 'i') $ map T.head (T.lines activeTimelog)
        loggedIn = length is > length os

    setStdin activeTimelog
    data1 <- run "ledger" ["-f", "-", "print", "complete"]
    data2 <- run "org2tc" ["/Users/johnw/doc/Tasks/FPComplete.txt"] -|-
             run "ledger" ["-f", "-", "print"]

    let combined = T.append data1 data2

    realHrs  <- balanceTotal args combined "this month"
    todayHrs <- balanceTotal [] combined "today"

    -- let firstDay  = LocalTime (fromGregorian 2013 1 1) midday
    --     secondDay = LocalTime (fromGregorian 2013 1 2) midday
    --     thirdDay  = LocalTime (fromGregorian 2013 1 3) midday

    let today     = toGregorian (localDay now)
        currHour  = fromIntegral (todHour (localTimeOfDay now)) / 3.0 :: Float
        (beg,end) = monthRange (fromIntegral (today^._1))
                               (fromIntegral (today^._2))
        -- jww (2013-01-16): FP Complete reduces the work month by one day
        workHrs   = getHours beg end - 8
        targetHrs = if null args
                    then (if isWeekendDay (localDay now) then 0 else currHour) +
                         getHours beg (localTimeToUTC (hoursToTimeZone 0) now)
                    else workHrs
        discrep   = realHrs - targetHrs
        indicator = if discrep < 0
                    then "\ESC[31m↓\ESC[0m"
                    else "\ESC[32m↑\ESC[0m"
        paceMark  = (realHrs * 100.0) / workHrs

    when debug $ liftIO $ do
        putStrLn $ "realHrs:     " ++ show realHrs
        putStrLn $ "todayHrs:    " ++ show todayHrs
        putStrLn $ "today:       " ++ show today
        putStrLn $ "currHour:    " ++ show currHour
        putStrLn $ "beg:         " ++ show beg
        putStrLn $ "end:         " ++ show end
        putStrLn $ "workHrs:     " ++ show workHrs
        putStrLn $ "targetHrs:   " ++ show targetHrs
        putStrLn $ "discrep:     " ++ show discrep
        putStrLn $ "indicator:   " ++ show indicator
        putStrLn $ "paceMark:    " ++ show paceMark
        putStrLn $ "length is:   " ++ show (length is)
        putStrLn $ "length os:   " ++ show (length os)
        putStrLn $ "loggedIn:    " ++ show loggedIn

    liftIO $ printf "%.1f%% %s%.1fh%s\n"
                    paceMark (T.unpack indicator) (abs discrep)
                    (if loggedIn
                     then printf " \ESC[30m⏱\ESC[0m%.2fh" todayHrs
                     else T.unpack "")
  where
    getHours beg end = fromIntegral ((countWorkDays beg end - 1) * 8)

-- Main.hs (hours) ends here
