{-# LANGUAGE RecordWildCards #-}

module Calc where

import qualified BAE
import qualified Budget
import           Budget (Interval(..))
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (NominalDiffTime)
import           Data.Time.LocalTime
import           Time
import qualified Timelog
import           Variant
-- import           Debug.Trace

data Budget = Budget
    { bStart                 :: ZonedTime
    , bNow                   :: ZonedTime
    , bEnd                   :: ZonedTime
    , bIdealExpected         :: Int
    , bIdealRemaining        :: Int
    , bIdealExpectedExact    :: NominalDiffTime
    , bIdealRemainingExact   :: NominalDiffTime
    , bIdealDaysLeft         :: Int
    , bIdealDaysLeftIncl     :: Int
    , bRealCompleted         :: NominalDiffTime
    , bRealExpected          :: NominalDiffTime
    , bRealExpectedInact     :: NominalDiffTime
    , bRealThisCompleted     :: NominalDiffTime
    , bRealThisRemaining     :: NominalDiffTime
    , bLoggedIn              :: Bool
    , bThisSym               :: Maybe BAE.WorkHours
    , bThereSym              :: Maybe BAE.WorkHours
    }

instance Show Budget where
  show Budget {..} = let v = variantToLisp in concat
    [ "((beg . ",                  v (TimeVal bStart), ")\n"
    , "(now . ",                   v (TimeVal bNow), ")\n"
    , "(end . ",                   v (TimeVal bEnd), ")\n"
    , "(ideal-total . ",           v (IntVal idealTotal), ")\n"
    , "(ideal-expected . ",        v (IntVal bIdealExpected), ")\n"
    , "(ideal-remaining . ",       v (IntVal bIdealRemaining), ")\n"
    , "(ideal-expected-exact . ",  v (DiffTimeVal bIdealExpectedExact), ")\n"
    , "(ideal-remaining-exact . ", v (DiffTimeVal bIdealRemainingExact), ")\n"
    , "(ideal-days-left . ",       v (IntVal bIdealDaysLeft), ")\n"
    , "(ideal-days-left-incl . ",  v (IntVal bIdealDaysLeftIncl), ")\n"
    , "(real-completed . ",        v (DiffTimeVal bRealCompleted), ")\n"
    , "(real-expected . ",         v (DiffTimeVal bRealExpected), ")\n"
    , "(real-expected-inact . ",   v (DiffTimeVal bRealExpectedInact), ")\n"
    , "(real-this-completed . ",   v (DiffTimeVal bRealThisCompleted), ")\n"
    , "(real-this-remaining . ",   v (DiffTimeVal bRealThisRemaining), ")\n"
    , "(real-discrepancy . ",      v (DiffTimeVal discrepancy), ")\n"
    , "(logged-in . ",             v (BoolVal bLoggedIn), ")\n"
    , "(this-sym . ",              v (OtherVal thisSym), ")\n"
    , "(there-sym . ",             v (OtherVal thereSym), ")\n"
    , ")\n"
    ]
   where
     discrepancy = bRealCompleted - bIdealExpectedExact
     idealTotal  = bIdealExpected + bIdealRemaining
     thisSym     = fromMaybe BAE.NotWorking bThisSym
     thereSym    = fromMaybe BAE.NotWorking bThereSym

calculateBudget :: ZonedTime -> String -> Budget
calculateBudget now activeTimelog =
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

    Budget {..}
  where
    bNow                  = now
    (bStart, bEnd)        = BAE.baeTwoWeekRange nowBAE
    bIdealExpected        = Budget.sumValues active
    bIdealRemaining       = Budget.sumValues future
    bIdealRemainingExact  = Budget.sumValues future'
    bIdealExpectedExact   = Budget.sumValues active'
    bIdealDaysLeft        = length future
    bIdealDaysLeftIncl    = length future' + maybe 1 (const 0) current
    bRealCompleted        = Budget.sumValues logHours
    bRealExpectedInact
        | null future = hoursLeft
        | otherwise   = hoursLeft / fromIntegral (length future)
    bRealExpected
        | bLoggedIn = activeExp
        | otherwise = bRealExpectedInact

    bRealThisCompleted    = Budget.sumValues today
    bRealThisRemaining    = activeExp - bRealThisCompleted
    bThisSym              = Budget.value <$> current

    -- Instead of BAE's work schedule using off-Fridays, I use a normal work
    -- pattern based on not taking off those Fridays.
    useMyNotionOfWorkTime = True

    workIntsWorkHours     = BAE.workIntervals useMyNotionOfWorkTime bStart bEnd
    workIntsDiffTime      = Budget.mapValues fromHours workIntsHours
    workIntsHours         = Budget.mapValues hoursToInt workIntsWorkHours
      where
        hoursToInt = BAE.workHoursToInt useMyNotionOfWorkTime

    (bLoggedIn, logHours) = Timelog.parseLogbook bNow activeTimelog

    -- Since I don't work 6-2 PST, I adjust real expectations to compute as if
    -- I were situated at the BAE office and working from there. This better
    -- models an ordinary 9-5 workday.
    nowBAE                = setTimeZone BAE.timeZoneBAE now'
      where
        secsAway = diffTimeZone BAE.timeZoneBAE (zonedTimeZone bNow)
        now'     = addZonedTime (- secsAway) bNow

    (active, future)      = Budget.activeIntervals nowBAE workIntsHours
    current               = Budget.current nowBAE workIntsWorkHours
    (active', future')    = Budget.divideIntervals nowBAE workIntsDiffTime

    bThereSym             = Budget.value <$> Budget.current bNow workIntsThere
      where
        workIntsThere = BAE.workIntervals False bStart bEnd

    (_, today)            = Budget.divideIntervals (setHour 0 bNow) logHours

    hoursLeft             = fromHours totalWork - bRealCompleted
      where
        totalWork = bIdealExpected + bIdealRemaining

    activeExp
        | bIdealDaysLeftIncl > 0 =
          (hoursLeft + bRealThisCompleted) / fromIntegral bIdealDaysLeftIncl
        | otherwise = hoursLeft + bRealThisCompleted
