module Hours.TimeTest (tests) where

import Data.Time.Clock (NominalDiffTime, UTCTime)
import Hours.Time
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Hours.Time"
        [ testGroup
            "toHours"
            [ testCase "3600 seconds = 1 hour" $
                toHours 3600 @?= 1.0
            , testCase "1800 seconds = 0.5 hours" $
                toHours 1800 @?= 0.5
            , testCase "0 seconds = 0 hours" $
                toHours 0 @?= 0.0
            , testCase "7200 seconds = 2 hours" $
                toHours 7200 @?= 2.0
            ]
        , testGroup
            "fromHours"
            [ testCase "1 hour = 3600 seconds" $
                fromHours 1 @?= (3600 :: NominalDiffTime)
            , testCase "8 hours = 28800 seconds" $
                fromHours 8 @?= (28800 :: NominalDiffTime)
            , testCase "0 hours = 0 seconds" $
                fromHours 0 @?= (0 :: NominalDiffTime)
            ]
        , testGroup
            "parseIso / formatIso round-trip"
            [ testCase "parses and re-formats a UTC time" $ do
                let timeStr = "2024-01-15 09:00:00 +0000"
                case parseIso timeStr :: Maybe UTCTime of
                    Nothing -> assertFailure "Failed to parse time string"
                    Just t -> do
                        let formatted = formatIso t
                        case parseIso formatted :: Maybe UTCTime of
                            Nothing ->
                                assertFailure $
                                    "Failed to re-parse: " ++ formatted
                            Just t' -> t @?= t'
            , testCase "parses time with timezone offset" $ do
                let timeStr = "2024-01-15 09:00:00 -0800"
                case parseIso timeStr :: Maybe UTCTime of
                    Nothing -> assertFailure "Failed to parse PST time string"
                    Just _ -> return ()
            ]
        ]
