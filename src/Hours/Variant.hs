{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hours.Variant where

import Data.Colour
import Data.Colour.SRGB
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word
import Hours.Time (toHours)
import Text.Printf (printf)

data Variant a
    = BoolVal Bool
    | DiffTimeVal NominalDiffTime
    | DoubleVal Double
    | IntVal Int
    | StringVal String
    | TimeVal UTCTime
    | ColourVal (Colour Double)
    | OtherVal a
    deriving Show

variantToLisp :: Show a => Variant a -> String
variantToLisp = \case
    BoolVal      True  -> "t"
    BoolVal      False -> "nil"
    DoubleVal    x     -> printf "%.4f" x
    DiffTimeVal  x     -> printf "%.4f" (toHours x)
    IntVal       x     -> show x
    StringVal    x     -> show x
    OtherVal     x     -> show x
    TimeVal      x     ->
        let secs = floor (utcTimeToPOSIXSeconds x) :: Int in
        printf "(%d %d 0 0)" (secs `div` 2^(16 :: Int))
                             (secs `mod` 2^(16 :: Int))
    ColourVal    x     ->
        let RGB rd gr bl :: RGB Word16 = toSRGBBounded x in
        printf "(%.6f %.6f %.6f)"
             (fromIntegral rd / 65535.0 :: Double)
             (fromIntegral gr / 65535.0 :: Double)
             (fromIntegral bl / 65535.0 :: Double)
