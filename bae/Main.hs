module Main where

import qualified BAE
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup (Semigroup((<>)))
import           Data.Time.LocalTime
import qualified Data.Yaml as Y
import           Hours.Input (IntervalFile(..), parseIso)
import           Hours.Time
import           Options.Applicative
import           System.IO.Unsafe

data Options = Options
    { now   :: ZonedTime
    , there :: Bool
    }

options :: Parser Options
options = Options
    <$> option parseTime (long "now"   <> help "Set the meaning of now"
                                       <> value (unsafePerformIO getZonedTime))
    <*> switch    (long "there" <> help "Give a remote notion of time")
  where
    parseTime :: ReadM ZonedTime
    parseTime = eitherReader parseIso

main :: IO ()
main = do
    opts <- execParser optsDef

    let -- Since I don't work 6-2 PST, I adjust real expectations to compute
        -- as if I were situated at the BAE office and working from there.
        -- This better models an ordinary 9-5 workday.
        nowThere = setTimeZone BAE.timeZoneBAE now'
          where
            secsAway = diffTimeZone BAE.timeZoneBAE (zonedTimeZone (now opts))
            now'     = addZonedTime (- secsAway) (now opts)

        moment = if there opts then nowThere else now opts
        (b, e) = BAE.baeTwoWeekRange moment
        ints   = BAE.workIntervals (there opts) b e

    B.putStrLn (Y.encode (IntervalFile b e nowThere False ints))
  where
    optsDef = info (helper <*> options)
        (fullDesc <> progDesc "Report BAE work periods"
                  <> header "bae-period")
