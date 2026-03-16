{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hours.BudgetTest (tests) where

import Data.These (These (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hours.Budget
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

-- Test instances for using Double as both time and value types
instance HasDelta Double Double where
    delta x y = x - y

instance Scalable Double Double where
    scale = (*)

mkInterval :: Double -> Double -> Double -> Interval Double Double
mkInterval = Interval

tests :: TestTree
tests =
    testGroup
        "Hours.Budget"
        [ testGroup
            "within"
            [ testCase "point inside interval" $
                within 5.0 (mkInterval 0.0 10.0 1.0) @?= True
            , testCase "point at begin (inclusive)" $
                within 0.0 (mkInterval 0.0 10.0 1.0) @?= True
            , testCase "point at end (exclusive)" $
                within 10.0 (mkInterval 0.0 10.0 1.0) @?= False
            , testCase "point before interval" $
                within (-1.0) (mkInterval 0.0 10.0 1.0) @?= False
            , testCase "point after interval" $
                within 11.0 (mkInterval 0.0 10.0 1.0) @?= False
            ]
        , testGroup
            "progress"
            [ testCase "midpoint gives 0.5" $
                progress (mkInterval 0.0 10.0 1.0) 5.0 @?= 0.5
            , testCase "begin gives 0.0" $
                progress (mkInterval 0.0 10.0 1.0) 0.0 @?= 0.0
            , testCase "quarter point" $
                progress (mkInterval 0.0 8.0 1.0) 2.0 @?= 0.25
            ]
        , testGroup
            "splitIntervals"
            [ testCase "empty list" $
                splitIntervals (5.0 :: Double) ([] :: [Interval Double Double])
                    @?= ([], Nothing, [])
            , testCase "single interval, point inside" $ do
                let (before, cur, aft) =
                        splitIntervals 5.0 [mkInterval 0.0 10.0 1.0]
                length before @?= 0
                cur @?= Just (mkInterval 0.0 10.0 1.0)
                length aft @?= 0
            , testCase "multiple intervals, point in middle" $ do
                let ints =
                        [ mkInterval 0.0 5.0 1.0
                        , mkInterval 5.0 10.0 2.0
                        , mkInterval 10.0 15.0 3.0
                        ]
                    (before, cur, aft) = splitIntervals 7.0 ints
                length before @?= 1
                cur @?= Just (mkInterval 5.0 10.0 2.0)
                length aft @?= 1
            , testCase "point between intervals" $ do
                let ints =
                        [ mkInterval 0.0 5.0 1.0
                        , mkInterval 10.0 15.0 2.0
                        ]
                    (before, cur, aft) = splitIntervals 7.0 ints
                length before @?= 1
                cur @?= Nothing
                length aft @?= 1
            ]
        , testGroup
            "sumValues"
            [ testCase "basic sum" $
                sumValues [mkInterval 0.0 5.0 1.0, mkInterval 5.0 10.0 2.0]
                    @?= 3.0
            , testCase "empty list" $
                sumValues ([] :: [Interval Double Double]) @?= 0.0
            , testCase "single interval" $
                sumValues [mkInterval 0.0 5.0 42.0] @?= 42.0
            ]
        , testGroup
            "distance"
            [ testCase "basic distance" $
                distance [mkInterval 0.0 5.0 1.0, mkInterval 10.0 15.0 2.0]
                    @?= (-10.0)
            , testCase "single interval" $
                distance [mkInterval 0.0 8.0 1.0] @?= (-8.0)
            ]
        , testGroup
            "breakAt"
            [ testCase "splits at midpoint with equal values" $ do
                let i = mkInterval 0.0 10.0 10.0
                case breakAt 5.0 i of
                    These l r -> do
                        value l @?= 5.0
                        value r @?= 5.0
                        end l @?= 5.0
                        begin r @?= 5.0
                    _ -> assertFailure "Expected These"
            , testCase "point before interval returns This" $ do
                let i = mkInterval 5.0 10.0 10.0
                case breakAt 3.0 i of
                    This _ -> return ()
                    _ -> assertFailure "Expected This"
            , testCase "point after interval returns That" $ do
                let i = mkInterval 0.0 5.0 10.0
                case breakAt 7.0 i of
                    That _ -> return ()
                    _ -> assertFailure "Expected That"
            ]
        , testGroup
            "properties"
            [ testProperty "breakAt preserves approximate total value" $
                property $ do
                    b <- forAll $ Gen.double (Range.linearFrac 0.0 100.0)
                    len <- forAll $ Gen.double (Range.linearFrac 0.1 100.0)
                    v <- forAll $ Gen.double (Range.linearFrac 0.1 100.0)
                    t <- forAll $ Gen.double (Range.linearFrac (b + 0.01) (b + len - 0.01))
                    let e = b + len
                        i = mkInterval b e v
                    case breakAt t i of
                        These left right -> do
                            let total = value left + value right
                            assert (abs (total - v) < 1e-6)
                        _ -> failure
            , testProperty "within is consistent with progress" $
                property $ do
                    b <- forAll $ Gen.double (Range.linearFrac 0.0 100.0)
                    len <- forAll $ Gen.double (Range.linearFrac 0.1 100.0)
                    t <- forAll $ Gen.double (Range.linearFrac (b + 0.01) (b + len - 0.01))
                    let e = b + len
                        i = mkInterval b e 1.0
                    assert (within t i)
                    let p = progress i t
                    assert (p > 0 && p < 1)
            ]
        ]
