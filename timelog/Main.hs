module Main where

import qualified Data.ByteString.Char8 as B
import           Data.Semigroup (Semigroup((<>)))
import           Data.Time.LocalTime
import qualified Data.Yaml as Y
import           Hours.Budget (mapValues)
import           Hours.Input (IntervalFile(..), WorkDay(NotWorking), parseIso)
import           Options.Applicative
import           System.IO.Unsafe
import           Timelog (parseLogbook)

data Options = Options
    { now     :: ZonedTime
    , rbegin  :: ZonedTime
    , rend    :: ZonedTime
    , timelog :: FilePath
    }

options :: Parser Options
options = Options
    <$> option parseTime (long "now"   <> help "Set the meaning of now"
                                       <> value (unsafePerformIO getZonedTime))
    <*> option parseTime (long "begin" <> help "Start of time range to read")
    <*> option parseTime (long "end"   <> help "End of time range to read")
    <*> strOption        (long "file"  <> help "File containing timelog data")
  where
    parseTime :: ReadM ZonedTime
    parseTime = eitherReader parseIso

main :: IO ()
main = do
    opts <- execParser optsDef

    input <- case timelog opts of
        "-"  -> getContents
        path -> readFile path

    let (logged, ints) = parseLogbook (now opts) input
        ints' = mapValues (\x -> (NotWorking, x)) ints

    B.putStrLn (Y.encode (IntervalFile (rbegin opts) (rend opts) (now opts)
                                       logged ints'))
  where
    optsDef = info (helper <*> options)
        (fullDesc <> progDesc "Report BAE work periods"
                  <> header "bae-period")
