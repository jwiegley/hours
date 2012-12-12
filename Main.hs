{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens hiding ( value )
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Shelly
import           Text.Printf

default (Integer, Text)

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
      targetHrs = fromRational (toRational (160 * fraction)) :: Float
      realHrs   = (/ 3600.0) $ read . T.unpack . T.init $
                  T.dropWhile (== ' ') (last (T.lines balance))

  liftIO $ printf "%.1fh (%.1f%%)\n" realHrs ((realHrs / targetHrs) * 100.0)

-- Main.hs (hours) ends here
