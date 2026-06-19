{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Property-based and unit tests for "ZKAlgebra.Multilinear".
--
-- These tests emphasize the evaluation-table indexing convention:
-- bit 0 is x1, bit 1 is x2, and so on.

module Test.Multilinear (multilinearTests) where

import qualified Data.Vector as V
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import ZKAlgebra.Field
import ZKAlgebra.Multilinear
import ZKAlgebra.Polynomial

-------------------------------------------------------------------------------
-- Test prime: F_97
-------------------------------------------------------------------------------

type F = Fp 97

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

genF :: Gen F
genF = mkFp <$> chooseInteger (0, 96)

genPoint :: Int -> Gen [F]
genPoint numVars = vectorOf numVars genF

genMLP :: Int -> Gen (MultilinearPoly F)
genMLP numVars =
  mlp numVars <$> vectorOf (2 ^ numVars) genF

genAnyMLP :: Gen (MultilinearPoly F)
genAnyMLP = do
  numVars <- chooseInt (0, 5)
  genMLP numVars

genPositiveDimMLP :: Gen (MultilinearPoly F)
genPositiveDimMLP = do
  numVars <- chooseInt (1, 5)
  genMLP numVars

genBoolPoint :: Int -> Gen [Bool]
genBoolPoint numVars = vectorOf numVars arbitrary

-------------------------------------------------------------------------------
-- Test tree
-------------------------------------------------------------------------------

multilinearTests :: TestTree
multilinearTests =
  testGroup
    "ZKAlgebra.Multilinear"
    [ constructionTests,
      evaluationTests,
      fixingTests,
      sumTests,
      unitTests
    ]

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

constructionTests :: TestTree
constructionTests =
  testGroup
    "Construction"
    [ testCase "construct 2-variable polynomial with 4 evaluations" $
        mlpNumVars (mlp 2 [10, 20, 30, 40] :: MultilinearPoly F) @?= 2,
      testProperty "evaluation table has length 2^numVars" $
        forAll genAnyMLP $ \f ->
          V.length (mlpEvals f) == 2 ^ mlpNumVars f
    ]

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

evaluationTests :: TestTree
evaluationTests =
  testGroup
    "Evaluation"
    [ testProperty "evaluating at a boolean point returns the stored value" $
        forAll (chooseInt (0, 5)) $ \numVars ->
          forAll (genMLP numVars) $ \f ->
            forAll (genBoolPoint numVars) $ \bools ->
              let point = boolPoint bools
                  idx = boolIndex bools
               in mleEval f point == mlpEvals f V.! idx,
      testProperty "0-variable polynomial evaluates to its only value" $
        forAll genF $ \a ->
          mleEval (mlp 0 [a] :: MultilinearPoly F) [] == a
    ]

-------------------------------------------------------------------------------
-- Fixing
-------------------------------------------------------------------------------

fixingTests :: TestTree
fixingTests =
  testGroup
    "Fixing variables"
    [ testProperty "mleEval f (r:rs) == mleEval (mleFix f r) rs" $
        forAll (chooseInt (1, 5)) $ \numVars ->
          forAll (genMLP numVars) $ \f ->
            forAll genF $ \r ->
              forAll (genPoint (numVars - 1)) $ \rs ->
                mleEval f (r : rs) == mleEval (mleFix f r) rs,
      testProperty "fixing reduces the variable count by one" $
        forAll (chooseInt (1, 5)) $ \numVars ->
          forAll (genMLP numVars) $ \f ->
            forAll genF $ \r ->
              mlpNumVars (mleFix f r) == numVars - 1
    ]

-------------------------------------------------------------------------------
-- Sums
-------------------------------------------------------------------------------

sumTests :: TestTree
sumTests =
  testGroup
    "Sums"
    [ testProperty "mleSum equals the sum of stored evaluations" $
        forAll genAnyMLP $ \f ->
          mleSum f == V.foldl' (+) 0 (mlpEvals f),
      testProperty "partial sum endpoints match x1 = 0 and x1 = 1 sums" $
        forAll genPositiveDimMLP $ \f ->
          let g = mlePartialSum f
           in polyEval g 0 == sumFirstBit 0 f
                && polyEval g 1 == sumFirstBit 1 f,
      testProperty "partial sum endpoints add up to total sum" $
        forAll genPositiveDimMLP $ \f ->
          let g = mlePartialSum f
           in polyEval g 0 + polyEval g 1 == mleSum f,
      testProperty "partial sum has degree at most one" $
        forAll genPositiveDimMLP $ \f ->
          degree (mlePartialSum f) <= 1
    ]

-------------------------------------------------------------------------------
-- Unit tests (hand-computed)
-------------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests (F_97)"
    [ testCase "2-variable boolean indexing uses bit 0 for x1" $
        let f = mlp 2 [10, 20, 30, 40] :: MultilinearPoly F
         in do
              mleEval f [0, 0] @?= 10
              mleEval f [1, 0] @?= 20
              mleEval f [0, 1] @?= 30
              mleEval f [1, 1] @?= 40,
      testCase "mleFix x1=2 pairs adjacent entries" $
        mleFix (mlp 2 [10, 20, 30, 40] :: MultilinearPoly F) 2
          @?= mlp 1 [30, 50],
      testCase "mleEval of 2-variable example at [2,3]" $
        mleEval (mlp 2 [10, 20, 30, 40] :: MultilinearPoly F) [2, 3]
          @?= 90,
      testCase "mlePartialSum for 2-variable example is 40 + 20X" $
        mlePartialSum (mlp 2 [10, 20, 30, 40] :: MultilinearPoly F)
          @?= poly [40, 20],
      testCase "mleSum sums all hypercube evaluations modulo p" $
        mleSum (mlp 2 [10, 20, 30, 40] :: MultilinearPoly F)
          @?= 3,
      testCase "3-variable boolean indexing" $
        mleEval (mlp 3 [0, 1, 2, 3, 4, 5, 6, 7] :: MultilinearPoly F) [1, 0, 1]
          @?= 5,
      testCase "3-variable evaluation at [2,3,4]" $
        mleEval (mlp 3 [0, 1, 2, 3, 4, 5, 6, 7] :: MultilinearPoly F) [2, 3, 4]
          @?= 24
    ]

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

boolPoint :: [Bool] -> [F]
boolPoint = map (\b -> if b then 1 else 0)

boolIndex :: [Bool] -> Int
boolIndex bools =
  sum [if bit then 2 ^ i else 0 | (i, bit) <- zip [(0 :: Int) ..] bools]

sumFirstBit :: Int -> MultilinearPoly F -> F
sumFirstBit bit f =
  V.ifoldl'
    (\acc i value -> if i `mod` 2 == bit then acc + value else acc)
    0
    (mlpEvals f)
