{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Semigroup (Semigroup((<>)))
import           Data.Time (UTCTime, getCurrentTime, getCurrentTimeZone)
import           Hours.Budget (mapValues)
import           Hours.Input (WorkDay(NotWorking), encodeIntervals)
import           Hours.Time (parseIso)
import           Options.Applicative
import           System.IO.Unsafe
import           Timelog (parseLogbook)

data Options = Options
    { now     :: UTCTime
    , rbegin  :: UTCTime
    , rend    :: UTCTime
    , timelog :: FilePath
    }
    deriving Show

options :: Parser Options
options = Options
    <$> option parseT
            (long "now"   <> help "Set the meaning of now"
                          <> value (unsafePerformIO getCurrentTime))
    <*> option parseT (long "begin" <> help "Start of time range to read")
    <*> option parseT (long "end"   <> help "End of time range to read")
    <*> strOption     (long "file"  <> help "File containing timelog data")
  where
    parseT :: ReadM UTCTime
    parseT = eitherReader parseIso

main :: IO ()
main = do
    opts  <- execParser $ info (helper <*> options)
        (fullDesc <> progDesc "Report BAE work periods"
                  <> header "bae-period")

    input <- case timelog opts of
        "-"  -> getContents
        path -> readFile path

    zone <- getCurrentTimeZone
    let (logged, ints) = parseLogbook zone (now opts) input

    BL.putStrLn (encodeIntervals (now opts) logged
                                 (mapValues (NotWorking,) ints))
