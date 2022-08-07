{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Data.Foldable
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Semigroup((<>)))
import           Data.Time.Clock (UTCTime, NominalDiffTime,
                                  getCurrentTime)
import           Data.Time.LocalTime (getCurrentTimeZone)
import           Diagrams.Backend.Cairo
import           Diagrams.Prelude hiding (Options, (<>), start,
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
    , height  :: Maybe Int
    , width   :: Maybe Int
    , diagram :: Maybe FilePath
    }

options :: Parser Options
options = Options
    <$> strOption (
          long "ideal" <>
          help "JSON file containing ideal intervals")
    <*> strOption (
          long "real" <> help "JSON file containing real intervals")
    <*> optional (option auto (
          long "height" <>
          help "Height of the graphical display"))
    <*> optional (option auto (
          long "width" <>
          help "Width of the graphical display"))
    <*> optional (strOption (
          long "diagram" <>
          help "Output graphic diagram as a .PNG file"))

main :: IO ()
main = do
    opts <- execParser $ info (helper <*> options)
        (fullDesc <> progDesc "Show hours worked so far"
                  <> header "hours - show hours worked so far")

    now    <- getCurrentTime
    zone   <- getCurrentTimeZone

    let decode f = fmap (fromMaybe (Input.defaultFile now))
                 . Input.decodeFile $ f opts
    ideals <- decode ideal
    reals  <- decode real

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

    putStrLn (hoursLispForm (loggedIn reals) stats)

    forM_ (diagram opts) $ \path -> do
        let w = fromMaybe 600 (width opts)
        renderCairo path (mkWidth (fromIntegral w))
            (hoursDiagram (fromMaybe 150 (height opts)) w
                 (loggedIn reals) stats)

idealProgress :: Budget t NominalDiffTime a -> Double
idealProgress Budget {..} = toHours bIdealExpectedExact / toHours bIdealTotal

realDiscrepancy :: Budget t NominalDiffTime a -> Double
realDiscrepancy b = (toHours (bRealRemaining b) -
                     toHours (bIdealRemainingExact b)) /
                    (16.0 - (12.0 * idealProgress b))

indicator :: Budget t u WorkDay -> String
indicator (bCurrentPeriod -> Holiday)    = "?"
indicator (bCurrentPeriod -> OffFriday)  = "!"
indicator (bCurrentPeriod -> HalfFriday) = "/"
indicator (bCurrentPeriod -> RegularDay) = "|"
indicator _ = "∙"

displayString :: Budget t NominalDiffTime WorkDay -> String
displayString budget@Budget {..} = printf "%s%.1f %s %s%.1f"
    (if bRealThisRemaining < 0 then "+" else "")
    (abs (toHours bRealThisRemaining))
    (indicator budget)
    (if bRealExpected < 0 then "+" else "")
    (abs (toHours bRealExpected))

totalString :: Budget t NominalDiffTime WorkDay -> String
totalString budget@Budget {..} = printf "%s%.1f %s %s%.1f"
    (if bRealRemaining < 0 then "+" else "")
    (abs (toHours bRealRemaining))
    (indicator budget)
    (if bExpectation < 0 then "+" else "")
    (abs (toHours bExpectation))

hoursLispForm :: Bool -> Budget UTCTime NominalDiffTime WorkDay -> String
hoursLispForm loggedIn b = concat
    [ "("
    , printf "(logged-in              . %s)\n"
             (variantToLisp (BoolVal loggedIn :: Variant ()))
    , printf "(ideal-progress         . %.4f)\n" (idealProgress b)
    , printf "(real-discrepancy       . %.4f)\n" (realDiscrepancy b)
    , printf "(display-string         . \"%s\")\n" (displayString b)
    , printf "(total-string           . \"%s\")\n" (totalString b)
    , printf "(indicator              . \"%s\")\n" (indicator b)
    , printf "(text-color             . %s)\n"
             (variantToLisp
              (ColourVal (textColor loggedIn b) :: Variant ()))
    , printf "(progress-color         . %s)\n"
             (variantToLisp
              (ColourVal (progressColor
                          (realDiscrepancy b)) :: Variant ()))
    , show b, ")" ]

hoursDiagram :: Int
             -> Int
             -> Bool
             -> Budget UTCTime NominalDiffTime WorkDay
             -> QDiagram Cairo V2 Double Any
hoursDiagram height width loggedIn b@Budget {..} =
    textDisplay <> (completionBar <> backgroundBar) # centerX
  where
    textDisplay     = text (if loggedIn
                            then displayString b
                            else totalString b)
                    # font "DejaVu Sans Mono"
                    # fontSize (local (20 * (barWidth / barHeight)))
                    # fontWeight FontWeightBold
                    # fc (textColor loggedIn b)

    barWidth        = fromIntegral width
    barHeight       = fromIntegral height
    completionWidth = barWidth * idealProgress b

    backgroundBar   = rect barWidth barHeight
                    # fc lightgrey
                    # alignL

    completionBar   = rect completionWidth barHeight
                    # fc (progressColor (realDiscrepancy b))
                    # alignL
