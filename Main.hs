module Main where

import           Data.Semigroup (Semigroup((<>)))
import           Data.Time (defaultTimeLocale)
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (formatTime)
import           Data.Time.LocalTime
import           Hours.Budget (mapValues)
import qualified Hours.Calc as Calc
import           Hours.Input (IntervalFile(..))
import qualified Hours.Input as Input
import           Hours.Time
import           Options.Applicative
import           System.Process (readProcessWithExitCode)

data Options = Options
    { ideal :: FilePath
    , real  :: FilePath
    , emacs :: Bool
    }

options :: Parser Options
options = Options
    <$> strOption (long "ideal" <> help "YAML file containing ideal intervals")
    <*> strOption (long "real"  <> help "YAML file containing real intervals")
    <*> switch    (long "emacs" <> help "Emit statistics in Emacs Lisp form")

main :: IO ()
main = do
    opts   <- execParser optsDef
    now    <- getZonedTime
    ideals <- Input.readFile (ideal opts)
    reals  <- Input.readFile (real opts)

    let (beg, end) = (start ideals, finish ideals)
        base       = setHour 0 now

    print $ Calc.calculateBudget beg end now base (nowThere ideals)
        (loggedIn reals) (intervals ideals) Input.NotWorking
        (mapValues snd (intervals reals))
  where
    optsDef = info (helper <*> options)
        (fullDesc <> progDesc "Show hours worked so far"
                  <> header "hours - show hours worked so far")

-- Main.hs (hours) ends here
