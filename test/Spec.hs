module Main where

import Test.Tasty

import qualified Hours.BudgetTest
import qualified Hours.InputTest
import qualified Hours.TimeTest

main :: IO ()
main =
    defaultMain $
        testGroup
            "Hours"
            [ Hours.BudgetTest.tests
            , Hours.TimeTest.tests
            , Hours.InputTest.tests
            ]
