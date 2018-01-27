module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Semigroup((<>)))
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.LocalTime (getCurrentTimeZone)
import           Hours.Budget (mapValues)
import qualified Hours.Calc as Calc
import           Hours.Input (IntervalFile(..))
import qualified Hours.Input as Input
import           Hours.Time
import           Options.Applicative

data Options = Options
    { ideal :: FilePath
    , real  :: FilePath
    , emacs :: Bool
    }

options :: Parser Options
options = Options
    <$> strOption (long "ideal" <> help "JSON file containing ideal intervals")
    <*> strOption (long "real"  <> help "JSON file containing real intervals")
    <*> switch    (long "emacs" <> help "Emit statistics in Emacs Lisp form")

main :: IO ()
main = do
    opts <- execParser $ info (helper <*> options)
        (fullDesc <> progDesc "Show hours worked so far"
                  <> header "hours - show hours worked so far")

    now    <- getCurrentTime
    zone   <- getCurrentTimeZone
    ideals <- fromMaybe (defaultFile now) <$> decodeFile (ideal opts)
    reals  <- fromMaybe (defaultFile now) <$> decodeFile (real opts)

    print $ Calc.calculateBudget
        (start ideals)
        (finish ideals)
        now
        (localDayStart zone now)
        (nowThere ideals)
        (loggedIn reals)
        (intervals ideals)
        Input.NotWorking
        (mapValues snd (intervals reals))
  where
    decodeFile :: FilePath -> IO (Maybe Input.IntervalFile)
    decodeFile p = either error return . A.eitherDecode =<< case p of
        "-"  -> BL.getContents
        path -> BL.readFile path

    defaultFile :: UTCTime -> Input.IntervalFile
    defaultFile now = Input.IntervalFile now now now False []

-- Main.hs (hours) ends here
