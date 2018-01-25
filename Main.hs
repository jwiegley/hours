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

data Options = Options
    { file  :: String
    , emacs :: Bool
    }

options :: Parser Options
options = Options
    <$> strOption (long "file" <> help "Active timelog file to use")
    <*> switch (long "emacs" <> help "Emit statistics in Emacs Lisp form")

main :: IO ()
main = do
    opts <- execParser optsDef
    now <- utcToZonedTime <$> getCurrentTimeZone <*> getCurrentTime

    let (beg, end) = BAE.baeTwoWeekRange now
        fmtTime    = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

    (_, activeTimelog, _) <-
        readProcessWithExitCode "org2tc"
            [file opts, "-s", fmtTime beg, "-e", fmtTime end] ""

    print $ Calc.calculateBudget now activeTimelog
  where
    optsDef = info (helper <*> options)
                   (fullDesc
                    <> progDesc "Show hours worked so far"
                    <> header "hours - show hours worked so far")

-- Main.hs (hours) ends here
