{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
import           Hours.Variant
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
        Nothing   -> putStrLn (hoursLispForm stats)
        Just path ->
            let w = fromMaybe 600 (width opts) in
            renderCairo path (mkWidth (fromIntegral w))
                (hoursDiagram (fromMaybe 150 (height opts)) w stats)

decodeFile :: FilePath -> IO (Maybe Input.IntervalFile)
decodeFile p = either error return . A.eitherDecode =<< case p of
    "-"  -> BL.getContents
    path -> BL.readFile path

defaultFile :: UTCTime -> Input.IntervalFile
defaultFile now = Input.IntervalFile now now now False []

dimColor :: (Num a, Ord a)
              => Colour a -> Colour a -> Colour a -> a -> Colour a
dimColor background ahead behind progress =
    withOpacity (if progress < 0
                 then behind
                 else ahead) (abs progress) `over` background

progressColor :: Budget t NominalDiffTime a -> Colour Double
progressColor = dimColor darkgrey red green . realDiscrepancy

textColor :: Budget t NominalDiffTime a -> Colour Double
textColor b | bLoggedIn b, bExpectation b < 0 = cyan
            | bLoggedIn b                     = yellow
            | otherwise                       = black

idealProgress :: Budget t NominalDiffTime a -> Double
idealProgress Budget {..} = toHours bIdealExpectedExact / toHours bIdealTotal

realDiscrepancy :: Budget t NominalDiffTime a -> Double
realDiscrepancy Budget {..} = (toHours bRealExpected - 8.0) / 2.0

indicator :: Budget t u WorkDay -> String
indicator (bCurrentPeriod -> Holiday)    = "?"
indicator (bCurrentPeriod -> OffFriday)  = "!"
indicator (bCurrentPeriod -> HalfFriday) = "/"
indicator (bCurrentPeriod -> RegularDay) = "|"
indicator _                             = "∙"

displayString :: Budget t NominalDiffTime WorkDay -> String
displayString budget@Budget {..} = printf "%.1f %s %s%.1f"
    (toHours bRealThisCompleted)
    (indicator budget)
    (if bExpectation < 0 then "+" else "")
    (abs (toHours bExpectation))

hoursLispForm :: Budget UTCTime NominalDiffTime WorkDay -> String
hoursLispForm b = concat
    [ "("
    , printf "(ideal-progress        . %.4f)\n" (idealProgress b)
    , printf "(real-discrepancy      . %.4f)\n" (realDiscrepancy b)
    , printf "(display-string        . \"%s\")\n" (displayString b)
    , printf "(indicator             . \"%s\")\n" (indicator b)
    , printf "(text-color            . %s)\n"
             (variantToLisp (ColourVal (textColor b) :: Variant ()))
    , printf "(progress-color        . %s)\n"
             (variantToLisp (ColourVal (progressColor b):: Variant ()))
    , show b
    , ")"
    ]

hoursDiagram :: Int
             -> Int
             -> Budget UTCTime NominalDiffTime WorkDay
             -> QDiagram Cairo V2 Double Any
hoursDiagram height width b@Budget {..} =
    textDisplay <> (completionBar <> backgroundBar) # centerX
  where
    textDisplay = text (displayString b)
        # font "DejaVu Mono"
        # fontSize (local (20 * (barWidth / barHeight)))
        # fontWeight FontWeightBold
        # fc (textColor b)

    barWidth        = fromIntegral width
    barHeight       = fromIntegral height
    completionWidth = barWidth * idealProgress b
    backgroundBar   = rect barWidth barHeight
                    # fc lightgrey # alignR
    completionBar   = rect completionWidth barHeight
                    # fc (progressColor b) # alignR
