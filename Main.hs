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
monthRange year month = do
  (mkUTCTime year month 1 0 0 0,
   mkUTCTime (if month == 12 then year + 1 else year)
             (if month == 12 then 1 else month + 1) 1 0 0 0)

countWorkDays :: UTCTime -> UTCTime -> Int
countWorkDays beg end = length $ takeWhile (<= end) $ starting beg $
                        recur daily >==> filter (WeekDays [Monday .. Friday])

main :: IO ()
main = shelly $ silently $ do
  data1 <- run "org2tc" ["/Users/johnw/doc/Tasks/todo.txt"] -|-
           run "ledger" ["-f", "-", "print", "complete"]
  data2 <- run "org2tc" ["/Users/johnw/doc/Tasks/FPComplete.txt"] -|-
           run "ledger" ["-f", "-", "print"]

  setStdin (T.append data1 data2)
  balance <- run "ledger" ["-f", "-", "-p", "this month", "--base", "bal"]

  now <- liftIO getCurrentTime
  let today     = toGregorian (utctDay now)
      day       = today^._3
      days      = gregorianMonthLength (today^._1) day
      fraction  = (secondsToDiffTime (toInteger (day * 86400)) +
                   utctDayTime now) /
                  secondsToDiffTime (toInteger (days * 86400))
      (beg,end) = monthRange (fromIntegral (today^._1))
                             (fromIntegral (today^._2))
      -- jww (2013-01-07): FP Complete reduces the work month by one day
      workHrs   = fromIntegral $ countWorkDays beg end * 8 - 8
      workHrsF  = fromRational (toRational workHrs) :: Float
      targetHrs = fromRational (toRational (workHrs * fraction)) :: Float
      realHrs   = (/ 3600.0) $ read . T.unpack . T.init $
                  T.dropWhile (== ' ') (last (T.lines balance))

  liftIO $ printf "%.1fh (%.1f%% ⊣ %.1f%% ⊢ %.1f%%)\n"
                  realHrs ((realHrs / targetHrs) * 100.0)
                  ((realHrs / workHrsF) * 100.0)
                  ((targetHrs / workHrsF) * 100.0)

-- Main.hs (hours) ends here
