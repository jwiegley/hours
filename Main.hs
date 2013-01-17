{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens hiding ( value )
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Recurrence
import           Prelude hiding (filter)
import           Shelly
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
    length $ takeWhile (<= end) $ starting beg $
        recur daily >==> filter (WeekDays [Monday .. Friday])

main :: IO ()
main = shelly $ silently $ do
    data1 <- run "org2tc" ["/Users/johnw/doc/Tasks/todo.txt"] -|-
             run "ledger" ["-f", "-", "print", "complete"]
    data2 <- run "org2tc" ["/Users/johnw/doc/Tasks/FPComplete.txt"] -|-
             run "ledger" ["-f", "-", "print"]

    setStdin (T.append data1 data2)
    balance <- run "ledger" ["-f", "-", "-p", "this month", "--base", "bal"]

    let debug   = False
        realHrs =
            case T.lines balance of
                [] -> 0.0 :: Float
                xs -> (/ 3600.0)
                   $ (read :: String -> Float) . T.unpack . T.init
                   $ T.dropWhile (== ' ') (last xs)
        -- jww (2013-01-16): FP Complete reduces the work month by one day
        adjustedHrs = realHrs + 8.0

    now <- liftIO $ getZonedTime

    -- let firstDay  = ZonedTime (LocalTime (fromGregorian 2013 1 1) midday)
    --                           (TimeZone (-360) True "CDT")
    --     secondDay  = ZonedTime (LocalTime (fromGregorian 2013 1 2) midday)
    --                           (TimeZone (-360) True "CDT")
    --     thirdDay  = ZonedTime (LocalTime (fromGregorian 2013 1 3) midday)
    --                           (TimeZone (-360) True "CDT")

    let today     = toGregorian (localDay (zonedTimeToLocalTime now))
        (beg,end) = monthRange (fromIntegral (today^._1))
                               (fromIntegral (today^._2))
        workHrs   = getHours beg end
        targetHrs = getHours beg (localTimeToUTC (hoursToTimeZone 0)
                                                 (zonedTimeToLocalTime now))
        discrep   = adjustedHrs - targetHrs
        indicator = if discrep < 0
                    then "\ESC[31m↓\ESC[0m"
                    else "\ESC[32m↑\ESC[0m"
        paceMark  = (adjustedHrs / workHrs) * 100.0

    when debug $ liftIO $ do
        putStrLn $ "realHrs:     " ++ show realHrs
        putStrLn $ "adjustedHrs: " ++ show adjustedHrs
        putStrLn $ "today:       " ++ show today
        putStrLn $ "beg:         " ++ show beg
        putStrLn $ "end:         " ++ show end
        putStrLn $ "workHrs:     " ++ show workHrs
        putStrLn $ "targetHrs:   " ++ show targetHrs
        putStrLn $ "discrep:     " ++ show discrep
        putStrLn $ "indicator:   " ++ show indicator
        putStrLn $ "paceMark:    " ++ show paceMark

    liftIO $ printf "%.1f%% %s%.1fh\n"
                    paceMark (T.unpack indicator) (abs discrep)
  where
    getHours beg end = fromIntegral ((countWorkDays beg end - 1) * 8)

-- Main.hs (hours) ends here
