{-# LANGUAGE LambdaCase #-}

module Hours.Variant where

import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Text.Printf (printf)
import Hours.Time

data Variant a
    = BoolVal Bool
    | DiffTimeVal NominalDiffTime
    | DoubleVal Double
    | IntVal Int
    | StringVal String
    | TimeVal ZonedTime
    | OtherVal a
    deriving Show

variantToLisp :: Show a => Variant a -> String
variantToLisp = \case
    BoolVal      True  -> "t"
    BoolVal      False -> "nil"
    DoubleVal    x     -> printf "%.1f" x
    DiffTimeVal  x     -> printf "%.1f" (toHours x)
    IntVal       x     -> show x
    StringVal    x     -> show x
    OtherVal     x     -> show x
    TimeVal      x     ->
        let secs = floor (utcTimeToPOSIXSeconds
                          (zonedTimeToUTC x)) :: Int in
        printf "(%d %d 0 0)" (secs `div` 2^(16 :: Int))
                             (secs `mod` 2^(16 :: Int))
