module Main where

import qualified BAE
import qualified Calc
import           Data.Semigroup (Semigroup((<>)))
import           Data.Time (defaultTimeLocale)
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (formatTime)
import           Data.Time.LocalTime
import           Options.Applicative
import           System.Process (readProcessWithExitCode)
import           Time
import qualified Timelog

data Options = Options
    { file  :: String
    , emacs :: Bool
    }

options :: Parser Options
options = Options
    <$> strOption (long "file"  <> help "Active timelog file to use")
    <*> switch    (long "emacs" <> help "Emit statistics in Emacs Lisp form")

main :: IO ()
main = do
    opts <- execParser optsDef

    now <- utcToZonedTime <$> getCurrentTimeZone <*> getCurrentTime

    let (beg, end) = BAE.baeTwoWeekRange now
        base       = setHour 0 now
        fmtTime    = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

    (_, activeTimelog, _) <-
        readProcessWithExitCode "org2tc"
            [file opts, "-s", fmtTime beg, "-e", fmtTime end] ""

    let idealHere  = BAE.workIntervals True  beg end
        idealThere = BAE.workIntervals False beg end

        (loggedIn, logHours) = Timelog.parseLogbook now activeTimelog

        -- Since I don't work 6-2 PST, I adjust real expectations to compute
        -- as if I were situated at the BAE office and working from there.
        -- This better models an ordinary 9-5 workday.
        nowThere = setTimeZone BAE.timeZoneBAE now'
          where
            secsAway = diffTimeZone BAE.timeZoneBAE (zonedTimeZone now)
            now'     = addZonedTime (- secsAway) now

    print $ Calc.calculateBudget beg end now base nowThere loggedIn
        idealHere idealThere BAE.NotWorking logHours
  where
    optsDef = info (helper <*> options)
        (fullDesc <> progDesc "Show hours worked so far"
                  <> header "hours - show hours worked so far")

-- Main.hs (hours) ends here
