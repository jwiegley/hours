{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Calc where

-- import Debug.Trace

import qualified Budget
import           Budget (Interval(..))
import           Data.Time.Clock (NominalDiffTime)
import           Data.Time.LocalTime
import           Variant

data Budget t u a = Budget
    { bStart                 :: t
    , bNow                   :: t
    , bEnd                   :: t
    , bIdealTotal            :: u
    , bIdealExpected         :: u
    , bIdealRemaining        :: u
    , bIdealExpectedExact    :: u
    , bIdealRemainingExact   :: u
    , bIdealPeriodsLeft      :: Int
    , bIdealPeriodsLeftIncl  :: Int
    , bRealCompleted         :: u
    , bRealExpected          :: u
    , bRealRemaining         :: u
    , bRealExpectedInact     :: u
    , bRealThisCompleted     :: u
    , bRealThisRemaining     :: u
    , bRealDiscrepancy       :: u
    , bLoggedIn              :: Bool
    , bThisSym               :: a
    , bThereSym              :: a
    }

instance (t ~ ZonedTime, u ~ NominalDiffTime, Show a) =>
    Show (Budget t u a) where
  show Budget {..} = let v = variantToLisp in concat
    [ "((beg                  . ", v (TimeVal      bStart),                ")\n"
    , "(now                   . ", v (TimeVal      bNow),                  ")\n"
    , "(end                   . ", v (TimeVal      bEnd),                  ")\n"
    , "(ideal-total           . ", v (DiffTimeVal  bIdealTotal),           ")\n"
    , "(ideal-expected        . ", v (DiffTimeVal  bIdealExpected),        ")\n"
    , "(ideal-remaining       . ", v (DiffTimeVal  bIdealRemaining),       ")\n"
    , "(ideal-expected-exact  . ", v (DiffTimeVal  bIdealExpectedExact),   ")\n"
    , "(ideal-remaining-exact . ", v (DiffTimeVal  bIdealRemainingExact),  ")\n"
    , "(ideal-days-left       . ", v (IntVal       bIdealPeriodsLeft),     ")\n"
    , "(ideal-days-left-incl  . ", v (IntVal       bIdealPeriodsLeftIncl), ")\n"
    , "(real-completed        . ", v (DiffTimeVal  bRealCompleted),        ")\n"
    , "(real-expected         . ", v (DiffTimeVal  bRealExpected),         ")\n"
    , "(real-remaining        . ", v (DiffTimeVal  bRealRemaining),        ")\n"
    , "(real-expected-inact   . ", v (DiffTimeVal  bRealExpectedInact),    ")\n"
    , "(real-this-completed   . ", v (DiffTimeVal  bRealThisCompleted),    ")\n"
    , "(real-this-remaining   . ", v (DiffTimeVal  bRealThisRemaining),    ")\n"
    , "(real-discrepancy      . ", v (DiffTimeVal  bRealDiscrepancy),      ")\n"
    , "(logged-in             . ", v (BoolVal      bLoggedIn),             ")\n"
    , "(this-sym              . ", v (OtherVal     bThisSym),              ")\n"
    , "(there-sym             . ", v (OtherVal     bThereSym),             ")\n"
    , ")\n"
    ]

calculateBudget :: (Functor f, Foldable f,
                   Functor g, Foldable g,
                   Functor h, Foldable h,
                   Ord t,
                   Fractional u,
                   Budget.HasDelta t u,
                   Budget.Scalable u u)
                => t
                -> t
                -> t
                -> t
                -> t
                -> Bool
                -> f (Interval t (a, u))
                -> g (Interval t (a, u))
                -> a
                -> h (Interval t u)
                -> Budget t u a
calculateBudget beg end now base now' loggedIn idealHere idealThere def real =
    Budget {..}
  where
    bStart                = beg
    bEnd                  = end
    bNow                  = now
    bLoggedIn             = loggedIn
    bIdealTotal           = bIdealExpected + bIdealRemaining
    bIdealExpected        = Budget.sumValues active
    bIdealRemaining       = Budget.sumValues future
    bIdealRemainingExact  = Budget.sumValues future'
    bIdealExpectedExact   = Budget.sumValues active'
    bIdealPeriodsLeft     = length future
    bIdealPeriodsLeftIncl = length future' + maybe 1 (const 0) current
    bRealCompleted        = Budget.sumValues real
    bRealExpected         | loggedIn  = activeExpectation
                          | otherwise = bRealExpectedInact
    bRealExpectedInact    | null future = bRealRemaining
                          | otherwise   = bRealRemaining
                                              / fromIntegral bIdealPeriodsLeft
    bRealRemaining        = bIdealTotal - bRealCompleted
    bRealThisCompleted    = Budget.sumValues today
    bRealThisRemaining    = activeExpectation - bRealThisCompleted
    bRealDiscrepancy      = bRealCompleted - bIdealExpectedExact
    bThisSym              = maybe def Budget.value current
    bThereSym             = maybe def Budget.value
                                (Budget.current bNow idealThereFst)

    idealHereFst          = Budget.mapValues fst idealHere
    idealHereSnd          = Budget.mapValues snd idealHere
    idealThereFst         = Budget.mapValues fst idealThere
    current               = Budget.current now' idealHereFst
    (active, future)      = Budget.activeIntervals now' idealHereSnd
    (active', future')    = Budget.divideIntervals now' idealHereSnd
    (_, today)            = Budget.divideIntervals base real

    activeExpectation
        | bIdealPeriodsLeftIncl > 0 =
              hours / fromIntegral bIdealPeriodsLeftIncl
        | otherwise = hours
      where
        hours = bRealRemaining + bRealThisCompleted

    -- trace ("bStart   = " ++ show bStart) $
    -- trace ("now      = " ++ show now) $
    -- trace ("bNow     = " ++ show bNow) $
    -- trace ("now'     = " ++ show now') $
    -- trace ("nowBAE   = " ++ show nowBAE) $
    -- trace ("bEnd     = " ++ show bEnd) $
    -- trace ("workints = " ++ Budget.showIntervals workIntsWorkHours) $
    -- trace ("active   = " ++ Budget.showIntervals active) $
    -- trace ("active'  = " ++ Budget.showIntervals active') $
    -- trace ("future   = " ++ Budget.showIntervals future) $
    -- trace ("future'  = " ++ Budget.showIntervals future') $
    -- trace ("logbook  = " ++ Budget.showIntervals logbook) $
    -- trace ("today    = " ++ Budget.showIntervals today) $
