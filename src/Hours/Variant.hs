{-# LANGUAGE LambdaCase #-}

module Hours.Variant where

import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Hours.Time (toHours)
import Text.Printf (printf)

data Variant a
    = BoolVal Bool
    | DiffTimeVal NominalDiffTime
    | DoubleVal Double
    | IntVal Int
    | StringVal String
    | TimeVal UTCTime
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
        let secs = floor (utcTimeToPOSIXSeconds x) :: Int in
        printf "(%d %d 0 0)" (secs `div` 2^(16 :: Int))
                             (secs `mod` 2^(16 :: Int))
