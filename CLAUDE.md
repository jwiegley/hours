# CLAUDE.md - AI Assistant Guide for the Hours Project

This file is for use by AI coding assistants. It provides essential context about this Haskell time tracking codebase to help AI assistants understand and modify the code effectively.

## Build and Development Commands

### Building the Project

```bash
# Build all executables
cabal build all

# Build specific executable
cabal build exe:process-hours
cabal build exe:work-periods
cabal build exe:timelog-periods

# Clean build
cabal clean

# Install dependencies
cabal build --dependencies-only all
```

Note: This project uses a custom time-recurrence dependency from GitHub (see cabal.project).

### Running the Executables

```bash
# Run directly with cabal
cabal run process-hours -- --ideal ideal.json --real real.json
cabal run work-periods -- --there
cabal run timelog-periods -- --file timeclock.dat

# Or after building, find executables in:
# dist-newstyle/build/*/ghc-*/hours-*/x/*/build/*/
```

### Testing

Currently, no test suite is configured. To add tests:
1. Add `test-suite` section to hours.cabal or package.yaml
2. Use `cabal test` to run

### Development Workflow

```bash
# Interactive development
cabal repl

# Load specific module in REPL
:load Hours.Calc

# Check types
:type calculateBudget

# Reload after changes
:reload
```

## 1. High-Level Architecture

The **hours** project is a budgeting algorithm with a time tracking use case. The core algorithm in `Hours.Budget` and `Hours.Calc` is resource-agnostic - it compares two lists of intervals (ideal vs. real) and computes statistics about progress. While currently used for time tracking, it could budget any divisible resource.

**Key Insight from README**: The final result is a brief Emacs modeline indicator like `6.2h | 8.4`, meaning:
- 6.2 hours left to work today to stay on track
- 8.4 hours required per remaining work period
- The `|` character indicates a work day (changes based on day type)
- Colors indicate if you're falling behind (red) or getting ahead (green)

### Architecture Pattern

The project follows a **layered functional architecture** with a pure functional core:

```
Executables (process-hours, work-periods, timelog-periods)
    ↓
Application Layer (Main.hs files - CLI, I/O coordination)
    ↓
Core Library (Hours.Calc → Hours.Budget → Hours.Time/Input/Variant)
```

### The Three Executables

1. **process-hours**: Main tool - compares ideal vs real intervals, outputs Lisp for Emacs
2. **work-periods**: Generates ideal work intervals based on schedule
3. **timelog-periods**: Converts timeclock format to interval JSON

## 2. Core Concepts

### The Interval Type

```haskell
data Interval t a = Interval
    { begin :: t    -- inclusive start
    , end   :: t    -- exclusive end
    , value :: a    -- associated value
    }
```

**Critical Properties**:
- **Half-open**: `[begin, end)` - begin is included, end is excluded
- **Generic**: Works with any time type `t` and value type `a`
- **Functorial**: Can map over values while preserving structure
- **Semigroup**: Intervals can be combined (union of ranges, combine values)

### The Budget Type

The `Budget` type has **18 fields** capturing all metrics:

```haskell
data Budget t u a = Budget
    { bStart                :: t    -- Budget period start
    , bNow                  :: t    -- Current time
    , bEnd                  :: t    -- Budget period end
    , bIdealTotal           :: u    -- Total ideal work for period
    , bIdealExpected        :: u    -- Ideal work completed so far (rounded)
    , bIdealRemaining       :: u    -- Ideal work left (rounded to periods)
    , bIdealExpectedExact   :: u    -- Exact progress (includes partial day)
    , bIdealRemainingExact  :: u    -- Exact remaining (includes partial day)
    , bIdealPeriodsLeft     :: Int  -- Full work periods remaining
    , bIdealPeriodsLeftIncl :: Int  -- Periods left including current partial
    , bRealCompleted        :: u    -- Actual work done
    , bRealExpected         :: u    -- Expected per period (when logged in)
    , bRealRemaining        :: u    -- Actual work left to do
    , bRealExpectedInact    :: u    -- Expected per period (when logged out)
    , bRealThisCompleted    :: u    -- Work done today
    , bRealThisRemaining    :: u    -- Work remaining today
    , bCurrentPeriod        :: a    -- Current period type (e.g., WorkDay)
    , bCurrentPeriodSpan    :: u    -- Duration of current period
    , bExpectation          :: u    -- Discrepancy (real - ideal)
    }
```

### WorkDay Types

```haskell
data WorkDay = NotWorking | Holiday | Weekend | OffFriday | HalfFriday | RegularDay
```

Each type determines expected hours and visual indicator:
- `RegularDay`: 8 hours, "|" indicator
- `HalfFriday`: 4 hours (BUG: currently 1000!), "/" indicator
- `Holiday`: 0 hours, "?" indicator
- `OffFriday`: 0 hours, "!" indicator

### Type Classes

**HasDelta** - Relates time types to their duration types:
```haskell
class HasDelta t u | t -> u where
    delta :: t -> t -> u
```
The functional dependency `t -> u` ensures each time type has exactly ONE duration type.

**Scalable** - Enables proportional scaling when splitting intervals:
```haskell
class Scalable t u where
    scale :: t -> u -> t
```

## 3. Module Architecture

### Dependency Hierarchy (bottom-up)

```
Hours.Budget (132 LOC) - Pure interval algebra, no dependencies
    ↓
Hours.Time (49 LOC) - Time-specific instances of type classes
    ↓
Hours.Input (86 LOC) - JSON serialization, WorkDay type
Hours.Variant (43 LOC) - Lisp output formatting
    ↓
Hours.Calc (148 LOC) - Budget calculation engine
```

### Module Responsibilities

- **Hours.Budget**: Generic interval operations (split, divide, progress, sum)
- **Hours.Calc**: The `calculateBudget` algorithm and color functions
- **Hours.Time**: `HasDelta UTCTime NominalDiffTime` instance
- **Hours.Input**: JSON parsing/generation, IntervalFile type
- **Hours.Variant**: Convert Haskell values to Emacs Lisp format

### Key Design Decisions

- **Functional dependencies** over type families for simplicity
- **Orphan instances** allowed with `-fno-warn-orphans` for practical reasons
- **Lisp output** for native Emacs integration (no JSON parsing needed)
- **Half-open intervals** to avoid boundary overlap issues

## 4. Common Modification Patterns

### Adding a New WorkDay Type

1. Edit `src/Hours/Input.hs`:
```haskell
data WorkDay = ... | FlexFriday  -- Add new constructor
```

2. Edit `work/Work.hs` to define hours:
```haskell
workHours _ FlexFriday = fromHours 6  -- 6-hour flex Friday
```

3. Update indicator in `Main.hs`:
```haskell
indicator (bCurrentPeriod -> FlexFriday) = "~"
```

4. Update JSON parsing if needed in `Hours.Input`

### Adding a New Budget Metric

1. Add field to Budget type in `src/Hours/Calc.hs`:
```haskell
data Budget t u a = Budget
    { ...
    , bNewMetric :: u  -- Document purpose
    }
```

2. Calculate in `calculateBudget`:
```haskell
bNewMetric = someCalculation
```

3. Add to Show instance for Lisp output:
```haskell
, "(new-metric . ", v (DiffTimeVal bNewMetric), ")\n"
```

### Adding a New Output Format

1. Create new module `src/Hours/Output/JSON.hs`:
```haskell
module Hours.Output.JSON where
import Hours.Calc

budgetToJSON :: Budget UTCTime NominalDiffTime a -> Value
budgetToJSON Budget{..} = object
    [ "ideal_progress" .= toHours bIdealExpectedExact / toHours bIdealTotal
    , ...
    ]
```

2. Add command-line option in executable
3. Import and use in Main.hs

### Integrating a New Data Source

1. Create parser in new module (e.g., `src/Hours/Input/Calendar.hs`)
2. Convert to `[Interval UTCTime NominalDiffTime]`
3. Add as alternative input in executable
4. Ensure intervals are sorted and non-overlapping

## 5. Gotchas and Pitfalls

### CRITICAL BUGS

**HalfFriday Hours Bug** (`work/Work.hs:72`):
```haskell
workHours _ HalfFriday = fromHours 1000  -- BUG: Should be 4!
```
This makes HalfFriday 1000 hours instead of 4. Fix carefully - may affect existing data.

**Overlapping Intervals Crash** (`src/Hours/Budget.hs:69`):
```haskell
case filter (within t) l of
    []  -> Nothing
    [a] -> Just a
    _   -> error "overlapping intervals"  -- Runtime crash!
```
Will crash the program if intervals overlap. Should return `Either` instead.

### Design Issues

**Large Budget Type**: 18 fields in flat structure. Consider refactoring into nested records:
```haskell
data Budget t u a = Budget
    { budgetPeriod :: Period t
    , budgetIdeal :: IdealMetrics u
    , budgetReal :: RealMetrics u
    , budgetCurrent :: CurrentMetrics u a
    }
```

**Hardcoded Configuration**: Holidays and timezone in `work/Work.hs`. Should externalize to config file.

**Error Swallowing**: JSON parsing in `Hours/Input.hs:85` uses `error`, losing parse details.

### Orphan Instances

- `Hours.Time`: Orphan `HasDelta UTCTime NominalDiffTime`
- `Hours.Input`: Orphan JSON instances for `Interval`

These are pragmatic choices but may cause issues if instances are defined elsewhere.

### Timezone Complexities

The system handles two timezones:
- Local timezone (where user is)
- Work timezone (PST hardcoded in `work/Work.hs`)

Be careful with daylight saving time transitions.

## 6. Testing and Verification

### Manual Testing Workflow

```bash
# Generate test data
cabal run work-periods -- --now "2024-01-15 09:00:00 PST" > test_ideal.json
echo "i 2024-01-15 09:00:00" | cabal run timelog-periods -- --file - > test_real.json
cabal run process-hours -- --ideal test_ideal.json --real test_real.json
```

### Validating Lisp Output

The output should be valid Emacs Lisp that can be read with:
```elisp
(read "(output from process-hours)")
```

### Adding a Test Suite

To add tests, update package.yaml or hours.cabal:
```yaml
tests:
  hours-test:
    main: Spec.hs
    source-dirs: test
    dependencies:
      - hours
      - tasty
      - tasty-hunit
      - tasty-quickcheck
```

## 7. Future Enhancement Ideas

### High Priority Improvements

1. **Fix HalfFriday bug** - Change 1000 to 4 hours
2. **Replace error with Either** - Proper error handling
3. **Add test suite** - QuickCheck properties + HUnit tests
4. **Externalize configuration** - Move holidays/timezone to config file

### Feature Additions

1. **Multiple Budget Periods** - Track daily/weekly/monthly simultaneously
2. **Categories/Tags** - Track hours by project or client
3. **Historical Analysis** - Trend analysis over time
4. **Forecasting** - "At current rate, you'll finish X days early/late"
5. **Web Interface** - REST API using Servant
6. **Database Backend** - PostgreSQL instead of JSON files

### Type Safety Enhancements

1. **Validated Intervals** - Smart constructors ensuring begin < end
2. **NonEmpty Lists** - Guarantee non-empty interval sets where needed
3. **Refined Types** - Use refined library for positive integers
4. **Phantom Types** - Track budget state at type level

## 8. Integration Guide

### Complete Workflow

```bash
# 1. Generate ideal schedule
work-periods --there > ideal.json

# 2. Track time in Org-mode
# CLOCK: [2024-01-15 Mon 09:00]--[2024-01-15 Mon 17:00] => 8:00

# 3. Convert Org to timeclock
org2tc -s "2024-01-01" todo.org > timeclock.dat

# 4. Convert timeclock to intervals
timelog-periods --file timeclock.dat > real.json

# 5. Calculate and display
process-hours --ideal ideal.json --real real.json
```

### Emacs Integration

The system outputs Lisp for Emacs consumption:
```elisp
;; In .emacs.d/init.el
(defvar jobhours-string "")

(defun jobhours-update ()
  (setq jobhours-string
        (shell-command-to-string "jobhours todo.org")))

(run-at-time 0 300 #'jobhours-update)  ; Every 5 minutes

;; Add to modeline
(setq-default mode-line-format
  (append mode-line-format (list 'jobhours-string)))
```

### Data Flow

```
Org-mode → org2tc → timeclock → timelog-periods → real.json
                                                       ↓
work-periods → ideal.json → process-hours → Lisp → Emacs modeline
```

## 9. Quick Reference

### Key Commands

```bash
# See ideal schedule for current month
work-periods --there

# Check current status
process-hours --ideal ideal.json --real real.json

# Parse timelog from stdin
cat timeclock.dat | timelog-periods --file -

# Override current time (for testing)
work-periods --now "2024-01-15 09:00:00 PST"
```

### Important Files

- `src/Hours/Calc.hs:96` - Core calculateBudget algorithm
- `src/Hours/Budget.hs` - Interval operations
- `work/Work.hs:72` - Work hours configuration (HAS BUG!)
- `Main.hs:119` - Lisp output formatting

### Key Functions

- `calculateBudget` - Main algorithm (`Hours.Calc`)
- `splitIntervals` - Divide at time point (`Hours.Budget`)
- `divideIntervals` - Split with proportional scaling (`Hours.Budget`)
- `workIntervals` - Generate ideal schedule (`work/Work.hs`)
- `parseLogbook` - Parse timeclock format (`timelog/Timelog.hs`)

### Type Class Instances

- `HasDelta UTCTime NominalDiffTime` - Time differences (`Hours.Time`)
- `Scalable NominalDiffTime NominalDiffTime` - Duration scaling (`Hours.Time`)

### Common Patterns

- Use `sumValues` for nested foldable structures
- Use `divideIntervals` when you need proportional splitting
- Use `splitIntervals` when you just need before/current/after
- Always check if an interval `within` before using `progress`

### Performance Notes

- Designed for ~20-30 intervals per month
- All operations are O(n) in interval count
- Not optimized for 1000s of intervals (use interval trees if needed)
- Lazy evaluation means only needed values are computed