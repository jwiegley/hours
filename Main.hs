{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Reader
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as B
import           Data.List
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (defaultTimeLocale)
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.LocalTime
import qualified Data.Time.Parsers as Time
import           Data.Time.Recurrence as R
import           Options.Applicative
import           Options.Applicative.Types (ReadM(..))
import           Shelly
import           System.IO.Unsafe
import           Text.Printf

data Variant
    = BoolVal Bool
    | DiffTimeVal NominalDiffTime
    | DoubleVal Double
    | IntVal Int
    | DateTripleVal (Integer, Int, Int)
    | LocalTimeVal LocalTime
    | MaybeTextVal (Maybe Text)
    | StringVal String
    | TimeVal UTCTime
    deriving (Eq, Ord, Show)

twoWeekStart :: UTCTime
twoWeekStart = mkUTCTime 2017 12 29 12 0 0

twoWeekDates :: [UTCTime]
twoWeekDates = starting twoWeekStart $ recur (daily `by` 14)

myTimeZone :: TimeZone
myTimeZone = TimeZone (-480) False "PST"

baeTimeZone :: TimeZone
baeTimeZone = TimeZone (-300) False "EST"

mkUTCTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
mkUTCTime year month day hour minute second =
    localTimeToUTC myTimeZone $ LocalTime
        (fromGregorian (fromIntegral year) (fromIntegral month)
                       (fromIntegral day))
        (TimeOfDay (fromIntegral hour) (fromIntegral minute)
                   (realToFrac second))

baeTwoWeekRange :: Integer -> Int -> Int -> Int -> (UTCTime, UTCTime)
baeTwoWeekRange year month day hour =
    let w = mkUTCTime year month day hour 0 0 in
    interval w twoWeekDates
  where
    interval t (x:y:xs)
        | y > t     = (x, y)
        | otherwise = interval t (y:xs)
    interval _ _ = error "Never occurs"

holidays :: Integer -> Int -> [Int]
holidays year month =
    fromMaybe [] $ join $ lookup month <$> lookup year
        [ (2014, [ (12, [25, 26]) ])
        , (2015, [ ( 1, [1]) ])
        , (2018, [ ( 1, [1]) ])
        ]

baeWorkHoursForDay :: Integer -> Int -> Int -> Int
baeWorkHoursForDay year month day =
    case fromGregorianValid year month day of
        Nothing -> error $ "workHoursForDay: invalid date: "
                       ++ show (year, month, day)
        Just (toWeekDate -> (_,_,dow))
            | day `elem` holidays year month -> 0
            | dow == 5  -> 4     -- Friday
            | otherwise -> 9

countWorkHours :: UTCTime -> UTCTime -> Int
countWorkHours beg end =
    sum $ map (uncurry3 baeWorkHoursForDay . toGregorian . utctDay)
        $ takeWhile (< end)
        $ starting beg
        $ recur daily >==> R.filter (WeekDays [Monday .. Friday])
  where
    uncurry3 f (a,b,c) = f a b c

isWeekendDay :: Day -> Bool
isWeekendDay day = let (_,_,dow) = toWeekDate day in dow == 6 || dow == 7

balanceTotal :: Text -> Text -> Sh Double
balanceTotal journal period' = do
    setStdin journal
    balance <- run "ledger" ["-f", "-", "--base", "-F", "%(scrub(total))\n"
                           , "-p", period', "--day-break", "bal"]
    return $ case T.lines balance of
        [] -> 0.0 :: Double
        xs -> (/ 3600.0)
           $ (read :: String -> Double) . T.unpack . T.init
           $ T.dropWhile (== ' ') (last xs)

data Options = Options
    { verbose  :: Bool
    , file     :: String
    , period   :: String
    , category :: String
    , archive  :: String
    , moment   :: LocalTime
    , emacs    :: Bool
    }

options :: Parser Options
options = Options
    <$> switch (long "verbose" <> help "Display statistics")
    <*> strOption (long "file" <> help "Active timelog file to use")
    <*> strOption (long "period" <> help "Period to report for" <> value "")

    <*> strOption (long "category"
                   <> help "Account/category to query from timelog"
                   <> value "")

    <*> strOption (long "archive" <> help "Archival timelog" <> value "")

    <*> option
          (ReadM $ asks $ fromJust . Atto.maybeResult .
               Time.parseWithDefaultOptions Time.defaultLocalTime . B.pack)
          (long "moment" <> help "Set notion of the current moment"
           <> value (unsafePerformIO $
                     (zonedTimeToLocalTime <$>) getZonedTime))

    <*> switch (long "emacs" <> help "Emit statistics in Emacs Lisp form")

main :: IO ()
main = execParser opts >>= doMain
  where
    opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "Show hours worked so far"
                 <> header "hours - show hours worked so far")

doMain :: Options -> IO ()
doMain opts = shelly $ (if verbose opts then verbosely else silently) $ do
    let per = if null (period opts)
              then Nothing
              else Just (T.pack (period opts))

    now <-
        if isNothing per
        then return (moment opts)
        else do
            dateString <- run "ledger" ["eval", "--now", fromJust per, "today"]
            return . fromJust $
                parseTimeM True defaultTimeLocale "%Y/%m/%d" (T.unpack dateString)

    let today     = toGregorian (localDay now)
        yr        = fromIntegral (today^._1)
        mon       = fromIntegral (today^._2)
        day       = fromIntegral (today^._3)
        mnight    = localTimeToUTC baeTimeZone
                        (LocalTime (localDay now) midnight)
        thishr    = todHour (localTimeOfDay now)
        (beg,end) = baeTwoWeekRange yr mon day thishr
        tdyBeg    = if today == toGregorian (utctDay beg)
                    then beg
                    else mnight
        thismom   = diffUTCTime (localTimeToUTC myTimeZone now) tdyBeg
        fmtTime   = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
                           . utcToLocalTime myTimeZone
        begs      = fmtTime beg
        ends      = fmtTime end
        fmtDate   = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"
                           . utcToLocalTime myTimeZone

    activeTimelog <- run "org2tc" [T.pack (file opts), "-s", begs, "-e", ends]
    let (is, os) = partition (== 'i') $ map T.head (T.lines activeTimelog)
        loggedIn = length is > length os

    setStdin activeTimelog
    data1 <- run "ledger" ["-f", "-", "--day-break", "print"]
    data2 <- if null (archive opts)
            then return ""
            else run "org2tc" [T.pack (archive opts), "-s", begs, "-e", ends]
                     -|- run "ledger" ["-f", "-", "--day-break", "print"]

    let combined = T.concat [data1, "\n", data2]

    realHrs  <- balanceTotal combined (fromMaybe ("since " <> fmtDate beg) per)
    todayHrs <- balanceTotal combined "today"

    let currHour  = fromIntegral (ceiling thismom :: Int) / 3600.0 / 3.0
        workHrs   = countWorkHours beg end
        targHrs   = countWorkHours beg mnight
        isWeekend = isWeekendDay (localDay now)
        targetHrs = if isNothing per
                    then (if isWeekend then 0 else currHour) +
                         fromIntegral targHrs
                    else fromIntegral workHrs
        discrep   = realHrs - targetHrs
        hoursLeft = fromIntegral workHrs - realHrs
        indicator = if discrep < 0
                    then "\ESC[0;31m↓\ESC[0m"
                    else "\ESC[0;32m↑\ESC[0m"
        paceMark  = (realHrs * 100.0) / fromIntegral workHrs

        details :: [(Text, Variant)]
        details =
            [ ("today",     DateTripleVal today)
            , ("now",       LocalTimeVal now)
            , ("beg",       TimeVal beg)
            , ("end",       TimeVal end)
            , ("midnight",  TimeVal mnight)
            , ("tdyBeg",    TimeVal tdyBeg)
            , ("currHour",  DoubleVal currHour)
            , ("targetHrs", DoubleVal targetHrs)
            , ("todayHrs",  DoubleVal todayHrs)
            , ("realHrs",   DoubleVal realHrs)
            , ("hoursLeft", DoubleVal hoursLeft)
            , ("discrep",   DoubleVal discrep)
            , ("period",    MaybeTextVal per)
            , ("days",      DiffTimeVal (diffUTCTime end beg / 3600 / 24))
            , ("isWeekend", BoolVal isWeekend)
            , ("workHrs",   IntVal workHrs)
            , ("targHrs",   IntVal targHrs)
            , ("indicator", StringVal indicator)
            , ("paceMark",  DoubleVal paceMark)
            , ("clockIns",  IntVal (length is))
            , ("clockOuts", IntVal (length os))
            , ("loggedIn",  BoolVal loggedIn) ]

    liftIO $ do
        when (verbose opts) $
            forM_ details $ \(n, v) -> printf "%s: %s" n (show v)

        if emacs opts
            then do
                putStr "("
                forM_ details $ \(n, v) -> do
                    putStr "("
                    putStr (T.unpack n)
                    putStr " . "
                    putStr $ case v of
                        BoolVal       True      -> "t"
                        BoolVal       False     -> "nil"
                        DiffTimeVal   x         -> show (floor x :: Int)
                        DoubleVal     x         -> show x
                        IntVal        x         -> show x
                        DateTripleVal (y, m, d) -> printf "(%d %d %d)" y m d
                        LocalTimeVal  x ->
                            let secs = floor (utcTimeToPOSIXSeconds
                                              (localTimeToUTC myTimeZone x))
                                           :: Int in
                            printf "(%d %d 0 0)"
                                (secs `div` 2^(16 :: Int))
                                (secs `mod` 2^(16 :: Int))
                        MaybeTextVal  (Just x)  -> show x
                        MaybeTextVal  Nothing   -> "nil"
                        StringVal     x         -> show x
                        TimeVal       x         ->
                            let secs = floor (utcTimeToPOSIXSeconds x) :: Int in
                            printf "(%d %d 0 0)"
                                (secs `div` 2^(16 :: Int))
                                (secs `mod` 2^(16 :: Int))
                    putStrLn ")"
                putStrLn ")"
            else printf "%s%s%.1fh %.0f%% (%.1fh)\n"
                (if loggedIn
                 then printf "\ESC[37m🕓\ESC[0m%.1fh " todayHrs
                 else T.unpack "")
                indicator (abs discrep) paceMark hoursLeft

-- Main.hs (hours) ends here
