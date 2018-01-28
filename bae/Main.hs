module Main where

import qualified BAE
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Semigroup (Semigroup(..))
import           Data.Time (UTCTime, getCurrentTime,
                            getCurrentTimeZone, addUTCTime)
import           Hours.Input (encodeIntervals)
import           Hours.Time
import           Options.Applicative
import           System.IO.Unsafe

data Options = Options
    { now   :: UTCTime
    , there :: Bool
    }
    deriving Show

options :: Parser Options
options = Options
    <$> option (eitherReader parseIso)
            (long "now"   <> help "Set the meaning of now"
                          <> value (unsafePerformIO getCurrentTime))
    <*> switch (long "there" <> help "Give a remote notion of time")

main :: IO ()
main = do
    opts <- execParser $ info (helper <*> options)
        (fullDesc <> progDesc "Report BAE work periods"
                  <> header "bae-period")

    zone <- getCurrentTimeZone

    let -- Since I don't work 6-2 PST, I adjust real expectations to compute
        -- as if I were situated at the BAE office and working from there.
        -- This better models an ordinary 9-5 workday.
        now' = addUTCTime (- secsAway) (now opts)
          where
            secsAway = diffTimeZone BAE.timeZoneBAE zone

        moment = if there opts then now' else now opts
        ints   = BAE.workIntervals (there opts) moment

    BL.putStrLn (encodeIntervals moment False ints)
