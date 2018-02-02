{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Colour
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Semigroup((<>)))
import           Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime)
import           Data.Time.LocalTime (getCurrentTimeZone)
import           Diagrams.Backend.Cairo
import           Diagrams.Prelude hiding (Options, (<>), start, over,
                                          option, height, width)
import           Diagrams.TwoD.Text
import           Hours.Budget (mapValues)
import           Hours.Calc
import           Hours.Input (IntervalFile(..), WorkDay(..))
import qualified Hours.Input as Input
import           Hours.Time
import           Options.Applicative
import           Text.Printf

data Options = Options
    { ideal   :: FilePath
    , real    :: FilePath
    , emacs   :: Bool
    , height  :: Maybe Int
    , width   :: Maybe Int
    , diagram :: Maybe FilePath
    }

options :: Parser Options
options = Options
    <$> strOption (long "ideal"   <> help "JSON file containing ideal intervals")
    <*> strOption (long "real"    <> help "JSON file containing real intervals")
    <*> switch    (long "emacs"   <> help "Emit statistics in Emacs Lisp form")
    <*> optional (option auto (long "height" <> help "Height of the graphical display"))
    <*> optional (option auto (long "width" <> help "Height of the graphical display"))
    <*> optional (strOption (
          long "diagram" <> help "Output graphic diagram as a .PNG file"))

main :: IO ()
main = do
    opts <- execParser $ info (helper <*> options)
        (fullDesc <> progDesc "Show hours worked so far"
                  <> header "hours - show hours worked so far")

    now    <- getCurrentTime
    zone   <- getCurrentTimeZone
    ideals <- fromMaybe (defaultFile now) <$> decodeFile (ideal opts)
    reals  <- fromMaybe (defaultFile now) <$> decodeFile (real opts)

    let stats = calculateBudget
            (start ideals)
            (finish ideals)
            now
            (localDayStart zone now)
            (nowThere ideals)
            (loggedIn reals)
            (intervals ideals)
            Input.NotWorking
            (mapValues snd (intervals reals))

    case diagram opts of
        Nothing   -> print stats
        Just path ->
            let w = fromMaybe 600 (width opts) in
            renderCairo path (mkWidth (fromIntegral w))
                (hoursDiagram (fromMaybe 150 (height opts)) w stats)
  where
    decodeFile :: FilePath -> IO (Maybe Input.IntervalFile)
    decodeFile p = either error return . A.eitherDecode =<< case p of
        "-"  -> BL.getContents
        path -> BL.readFile path

    defaultFile :: UTCTime -> Input.IntervalFile
    defaultFile now = Input.IntervalFile now now now False []

dimColor :: (Fractional a, RealFrac a, Ord a)
         => Colour a -> Colour a -> a -> Colour a
dimColor background color factor = withOpacity color factor `over` background

hoursDiagram :: Int
             -> Int
             -> Budget UTCTime NominalDiffTime WorkDay
             -> QDiagram Cairo V2 Double Any
hoursDiagram height width Budget {..}
    = textDisplay <> (completionBar <> backgroundBar) # centerX
  where
    textDisplay = text displayString
        # font "DejaVu Mono"
        # fontSize (local (20 * (barWidth / barHeight)))
        # fontWeight FontWeightBold
        # fc textColor

    completionBar = rect completionWidth barHeight # fc foregroundColor # alignR
    backgroundBar = rect barWidth barHeight # fc barColor # alignR

    displayString = printf "%.1f %s %s%.1f"
        (toHours bRealThisCompleted)
        (case bCurrentPeriod of
             Holiday    -> "?"
             OffFriday  -> "!"
             HalfFriday -> "/"
             RegularDay -> "|"
             _          -> "∙")
        (if expectation < 0 then "+" else "")
        (toHours (abs expectation))

    textColor       | bLoggedIn, expectation < 0 = cyan
                    | bLoggedIn = yellow
                    | otherwise = black
    foregroundColor = dimColor darkgrey
        (if discrepancy < 0
         then behindColor
         else aheadColor)
        (abs discrepancy)
    barColor        = lightgrey
    aheadColor      = green
    behindColor     = red

    discrepancy   = (toHours bRealExpected - 8.0) / 2.0
    expectation   = bRealRemaining - bIdealRemaining
    idealProgress = toHours bIdealExpectedExact / toHours bIdealTotal

    barWidth :: Double
    barWidth = fromIntegral width

    barHeight :: Double
    barHeight = fromIntegral height

    completionWidth :: Double
    completionWidth = barWidth * idealProgress
