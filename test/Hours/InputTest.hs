module Hours.InputTest (tests) where

import Data.Aeson (decode, encode)
import Hours.Input
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Hours.Input"
        [ testGroup
            "WorkDay JSON round-trip"
            [ testCase "RegularDay" $
                decode (encode RegularDay) @?= Just RegularDay
            , testCase "Holiday" $
                decode (encode Holiday) @?= Just Holiday
            , testCase "Weekend" $
                decode (encode Weekend) @?= Just Weekend
            , testCase "OffFriday" $
                decode (encode OffFriday) @?= Just OffFriday
            , testCase "HalfFriday" $
                decode (encode HalfFriday) @?= Just HalfFriday
            , testCase "NotWorking" $
                decode (encode NotWorking) @?= Just NotWorking
            ]
        ]
