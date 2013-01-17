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
    let realHrs = (/ 3600.0)
                $ (read :: String -> Float) . T.unpack . T.init
                $ T.dropWhile (== ' ') (last (T.lines balance))

    now <- liftIO $ getZonedTime

    let today     = toGregorian (localDay (zonedTimeToLocalTime now))
        (beg,end) = monthRange (fromIntegral (today^._1))
                               (fromIntegral (today^._2))
        workHrs   = getHours beg end
        targetHrs = getHours beg (localTimeToUTC (hoursToTimeZone 0)
                                                 (zonedTimeToLocalTime now))
        discrep   = realHrs - targetHrs
        indicator = if discrep < 0
                    then "\ESC[31m↓\ESC[0m"
                    else "\ESC[32m↑\ESC[0m"
        paceMark  = (realHrs / workHrs) * 100.0

    -- liftIO $ putStrLn $ "realHrs:   " ++ show realHrs
    -- liftIO $ putStrLn $ "today:     " ++ show today
    -- liftIO $ putStrLn $ "beg:       " ++ show beg
    -- liftIO $ putStrLn $ "end:       " ++ show end
    -- liftIO $ putStrLn $ "workHrs:   " ++ show workHrs
    -- liftIO $ putStrLn $ "targetHrs: " ++ show targetHrs
    -- liftIO $ putStrLn $ "discrep:   " ++ show discrep
    -- liftIO $ putStrLn $ "indicator: " ++ show indicator
    -- liftIO $ putStrLn $ "paceMark:  " ++ show paceMark

    liftIO $ printf "%.1f%% %s%.1fh\n" paceMark (T.unpack indicator) discrep
  where
    getHours beg end = -- FP Complete reduces the work month by one day
                       fromIntegral ((countWorkDays beg end - 1) * 8)

-- Main.hs (hours) ends here
