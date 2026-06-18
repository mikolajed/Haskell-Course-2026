{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Property-based and unit tests for "ZKAlgebra.NTT".
--
-- The tests compare the transform against direct polynomial evaluation and
-- compare NTT multiplication against the Phase 2 naive multiplication.

module Test.NTT (nttTests) where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import ZKAlgebra.Field
import ZKAlgebra.NTT
import ZKAlgebra.Polynomial

-------------------------------------------------------------------------------
-- Test prime: F_97
-------------------------------------------------------------------------------

type F = Fp 97
type NTTFriendly = Fp 998244353

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

genF :: Gen F
genF = mkFp <$> chooseInteger (0, 96)

genNttInput :: Gen [F]
genNttInput = do
  n <- elements [1, 2, 4, 8, 16, 32]
  vectorOf n genF

genPoly :: Int -> Gen (Poly F)
genPoly maxDegree = do
  len <- chooseInt (0, maxDegree + 1)
  poly <$> vectorOf len genF

-------------------------------------------------------------------------------
-- Test tree
-------------------------------------------------------------------------------

nttTests :: TestTree
nttTests =
  testGroup
    "ZKAlgebra.NTT"
    [ rootTests,
      transformTests,
      multiplicationTests,
      nttFriendlyPrimeTests,
      unitTests
    ]

-------------------------------------------------------------------------------
-- Roots of unity
-------------------------------------------------------------------------------

rootTests :: TestTree
rootTests =
  testGroup
    "Roots of unity"
    [ testProperty "primitive power-of-two root has exact order" $
        forAll (elements [1, 2, 4, 8, 16, 32]) $ \n ->
          let root = primitiveRoot @97 n
           in root ^ n == 1 && (n == 1 || root ^ (n `div` 2) /= 1),
      testCase "primitive 4th root in F_97 squares to -1" $
        let root = primitiveRoot @97 4
         in do
              root ^ (2 :: Int) @?= (96 :: F)
              root ^ (4 :: Int) @?= (1 :: F)
    ]

-------------------------------------------------------------------------------
-- Transform
-------------------------------------------------------------------------------

transformTests :: TestTree
transformTests =
  testGroup
    "Transform"
    [ testProperty "inverse transform is a round-trip" $
        forAll genNttInput $ \xs ->
          let root = primitiveRoot @97 (length xs)
           in invNtt root (ntt root xs) == xs,
      testProperty "forward transform agrees with direct evaluation" $
        forAll genNttInput $ \xs ->
          let root = primitiveRoot @97 (length xs)
              p = poly xs
              points = [root ^ k | k <- [0 .. length xs - 1]]
           in ntt root xs == map (polyEval p) points
    ]

-------------------------------------------------------------------------------
-- Polynomial multiplication
-------------------------------------------------------------------------------

multiplicationTests :: TestTree
multiplicationTests =
  testGroup
    "Polynomial multiplication"
    [ testProperty "polyMulNTT agrees with naive polyMul" $
        forAll (genPoly 8) $ \p ->
          forAll (genPoly 8) $ \q ->
            polyMulNTT p q == polyMul p q,
      testProperty "eval(polyMulNTT p q) == eval(p) * eval(q)" $
        forAll (genPoly 8) $ \p ->
          forAll (genPoly 8) $ \q ->
            forAll genF $ \x ->
              polyEval (polyMulNTT p q) x == polyEval p x * polyEval q x
    ]

-------------------------------------------------------------------------------
-- NTT-friendly prime
-------------------------------------------------------------------------------

nttFriendlyPrimeTests :: TestTree
nttFriendlyPrimeTests =
  testGroup
    "F_998244353"
    [ testCase "primitive root exists for length 1024" $
        let root = primitiveRoot @998244353 1024
         in do
              root ^ (1024 :: Int) @?= (1 :: NTTFriendly)
              root ^ (512 :: Int) /= (1 :: NTTFriendly) @? "root has exact order",
      testCase "polyMulNTT agrees with naive multiplication" $
        let p = poly [3, 1, 0, 1, 5, 9, 2] :: Poly NTTFriendly
            q = poly [1, 1, 4, 0, 7] :: Poly NTTFriendly
         in polyMulNTT p q @?= polyMul p q
    ]

-------------------------------------------------------------------------------
-- Unit tests (hand-computed)
-------------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests (F_97)"
    [ testCase "NTT of [1,2,3,4] with root 22" $
        ntt (22 :: F) [1, 2, 3, 4] @?= [10, 51, 95, 42],
      testCase "inverse NTT recovers [1,2,3,4]" $
        invNtt (22 :: F) [10, 51, 95, 42] @?= [1, 2, 3, 4],
      testCase "polyMulNTT computes (3 + x + x^3)(1 + x)" $
        polyMulNTT (poly [3, 1, 0, 1] :: Poly F) (poly [1, 1])
          @?= poly [3, 4, 1, 1, 1],
      testCase "polyMulNTT by zero polynomial is zero" $
        polyMulNTT (poly [1, 2, 3] :: Poly F) (Poly [])
          @?= Poly []
    ]
