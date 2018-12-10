{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Hours.Calc where

-- import Debug.Trace

import           Data.Colour
import           Data.Colour.Names
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Hours.Budget (Interval(..))
import qualified Hours.Budget as Budget
import           Hours.Variant

dimColor :: (Num a, Ord a)
         => Colour a -> Colour a -> Colour a -> a -> Colour a
dimColor background ahead behind progress =
    withOpacity (if progress < 0
                 then behind
                 else ahead) (abs progress) `over` background

progressColor :: Double -> Colour Double
progressColor = dimColor darkgrey red green

textColor :: (Ord u, Num u) => Bool -> Budget t u a -> Colour Double
textColor loggedIn b
    | loggedIn, bRealThisRemaining b < 0 = cyan
    | loggedIn = yellow
    | otherwise = gray

data Budget t u a = Budget
    { bStart                :: t
    , bNow                  :: t
    , bEnd                  :: t
    , bIdealTotal           :: u
    , bIdealExpected        :: u
    , bIdealRemaining       :: u
    , bIdealExpectedExact   :: u
    , bIdealRemainingExact  :: u
    , bIdealPeriodsLeft     :: Int
    , bIdealPeriodsLeftIncl :: Int
    , bRealCompleted        :: u
    , bRealExpected         :: u
    , bRealRemaining        :: u
    , bRealExpectedInact    :: u
    , bRealThisCompleted    :: u
    , bRealThisRemaining    :: u
    , bCurrentPeriod        :: a
    , bCurrentPeriodSpan    :: u
    , bExpectation          :: u
    }

instance (t ~ UTCTime, u ~ NominalDiffTime, Show a) =>
    Show (Budget t u a) where
  show Budget {..} = let v = variantToLisp in concat
    [ "(beg                    . ", v (TimeVal      bStart),                ")\n"
    , "(now                    . ", v (TimeVal      bNow),                  ")\n"
    , "(end                    . ", v (TimeVal      bEnd),                  ")\n"
    , "(ideal-total            . ", v (DiffTimeVal  bIdealTotal),           ")\n"
    , "(ideal-expected         . ", v (DiffTimeVal  bIdealExpected),        ")\n"
    , "(ideal-remaining        . ", v (DiffTimeVal  bIdealRemaining),       ")\n"
    , "(ideal-expected-exact   . ", v (DiffTimeVal  bIdealExpectedExact),   ")\n"
    , "(ideal-remaining-exact  . ", v (DiffTimeVal  bIdealRemainingExact),  ")\n"
    , "(ideal-days-left        . ", v (IntVal       bIdealPeriodsLeft),     ")\n"
    , "(ideal-days-left-incl   . ", v (IntVal       bIdealPeriodsLeftIncl), ")\n"
    , "(real-completed         . ", v (DiffTimeVal  bRealCompleted),        ")\n"
    , "(real-remaining         . ", v (DiffTimeVal  bRealRemaining),        ")\n"
    , "(real-expected          . ", v (DiffTimeVal  bRealExpected),         ")\n"
    , "(real-expected-inact    . ", v (DiffTimeVal  bRealExpectedInact),    ")\n"
    , "(real-this-completed    . ", v (DiffTimeVal  bRealThisCompleted),    ")\n"
    , "(real-this-remaining    . ", v (DiffTimeVal  bRealThisRemaining),    ")\n"
    , "(current-period         . ", v (OtherVal     bCurrentPeriod),        ")\n"
    , "(current-period-span    . ", v (DiffTimeVal  bCurrentPeriodSpan),    ")\n"
    , "(expectation            . ", v (DiffTimeVal  bExpectation),          ")\n"
    ]

calculateBudget :: (Functor f, Foldable f,
                   Functor g, Foldable g,
                   Ord t,
                   Fractional u,
                   Budget.HasDelta t u,
                   Budget.Scalable u u
                  , Show a, Show t, Show u
                  )
                => t
                -> t
                -> t
                -> t
                -> t
                -> Bool
                -> f (Interval t (a, u))
                -> a
                -> g (Interval t u)
                -> Budget t u a
calculateBudget start finish now base now' loggedIn ideal def real =
    -- trace ("now      = " ++ show now) $
    -- trace ("now'     = " ++ show now') $
    -- trace ("bNow     = " ++ show bNow) $
    -- trace ("bStart   = " ++ show bStart) $
    -- trace ("bEnd     = " ++ show bEnd) $
    -- trace ("ideal    = " ++ Budget.showIntervals ideal) $
    -- trace ("real     = " ++ Budget.showIntervals real) $
    -- trace ("active   = " ++ Budget.showIntervals active) $
    -- trace ("active'  = " ++ Budget.showIntervals active') $
    -- trace ("future   = " ++ Budget.showIntervals future) $
    -- trace ("future'  = " ++ Budget.showIntervals future') $
    -- trace ("today    = " ++ Budget.showIntervals today) $

    Budget {..}
  where
    bStart                = start
    bEnd                  = finish
    bNow                  = now
    bIdealTotal           = bIdealExpected + bIdealRemaining
    bIdealExpected        = Budget.sumValues active
    bIdealRemaining       = Budget.sumValues future
    bIdealRemainingExact  = Budget.sumValues future'
    bIdealExpectedExact   = Budget.sumValues active'
    bIdealPeriodsLeft     = length future
    bIdealPeriodsLeftIncl = length future' + maybe 1 (const 0) current
    bRealCompleted        = Budget.sumValues real
    bRealExpected         | loggedIn, Just _ <- current = activeExpectation
                          | otherwise = bRealExpectedInact
    bRealExpectedInact    | null future = bRealRemaining
                          | otherwise   = bRealRemaining
                                              / fromIntegral bIdealPeriodsLeft
    bRealRemaining        = bIdealTotal - bRealCompleted
    bRealThisCompleted    = Budget.sumValues today
    bRealThisRemaining    | bIdealPeriodsLeftIncl > 1 =
                            activeExpectation - bRealThisCompleted
                          | otherwise = bRealRemaining
    bExpectation          = bRealRemaining - bIdealRemaining
    bCurrentPeriodSpan    = maybe 0 (\p -> Budget.delta (end p) (begin p)) current
    bCurrentPeriod        = maybe def Budget.value current

    idealFst              = Budget.mapValues fst ideal
    idealSnd              = Budget.mapValues snd ideal
    current               = Budget.current now' idealFst
    (active, future)      = Budget.activeIntervals now' idealSnd
    (active', future')    = Budget.divideIntervals now' idealSnd
    (_, today)            = Budget.divideIntervals base real

    activeExpectation
        | bIdealPeriodsLeftIncl > 1 =
              (bRealRemaining + bRealThisCompleted)
                  / fromIntegral bIdealPeriodsLeftIncl
        | otherwise = bRealRemaining
