{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}

-- |
-- Property-based and unit tests for "ZKAlgebra.Field".
--
-- The property tests directly encode the field axioms:
-- every property that holds here is a mathematical theorem about F_p,
-- verified empirically on random inputs.

module Test.Field (fieldTests) where

import Data.Proxy           (Proxy(..))
import GHC.TypeNats         (KnownNat, natVal)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import ZKAlgebra.Field

-------------------------------------------------------------------------------
-- Arbitrary instance for Fp (orphan — lives in tests only)
-------------------------------------------------------------------------------

instance KnownNat p => Arbitrary (Fp p) where
  arbitrary = mkFp <$> chooseInteger (0, toInteger (natVal (Proxy :: Proxy p)) - 1)
  shrink (Fp n) = [ Fp n' | n' <- shrink n, n' >= 0 ]

-------------------------------------------------------------------------------
-- Test prime: F_97
-------------------------------------------------------------------------------

type F = Fp 97

-------------------------------------------------------------------------------
-- Test tree
-------------------------------------------------------------------------------

fieldTests :: TestTree
fieldTests = testGroup "ZKAlgebra.Field"
  [ additionTests
  , multiplicationTests
  , distributivityTests
  , fieldSpecificTests
  , utilityTests
  , unitTests
  , secondPrimeTests
  ]

-------------------------------------------------------------------------------
-- Additive group axioms
-------------------------------------------------------------------------------

additionTests :: TestTree
additionTests = testGroup "Addition (additive group)"
  [ testProperty "commutativity: a + b == b + a" $
      \(a :: F) b -> a + b == b + a

  , testProperty "associativity: (a+b)+c == a+(b+c)" $
      \(a :: F) b c -> (a + b) + c == a + (b + c)

  , testProperty "identity: a + 0 == a" $
      \(a :: F) -> a + 0 == a

  , testProperty "inverse: a + (-a) == 0" $
      \(a :: F) -> a + negate a == 0

  , testProperty "subtraction: a - b == a + (-b)" $
      \(a :: F) b -> a - b == a + negate b

  , testProperty "double negation: -(-a) == a" $
      \(a :: F) -> negate (negate a) == a
  ]

-------------------------------------------------------------------------------
-- Multiplicative group axioms
-------------------------------------------------------------------------------

multiplicationTests :: TestTree
multiplicationTests = testGroup "Multiplication (multiplicative group)"
  [ testProperty "commutativity: a * b == b * a" $
      \(a :: F) b -> a * b == b * a

  , testProperty "associativity: (a*b)*c == a*(b*c)" $
      \(a :: F) b c -> (a * b) * c == a * (b * c)

  , testProperty "identity: a * 1 == a" $
      \(a :: F) -> a * 1 == a

  , testProperty "inverse: a /= 0 ==> a * a^(-1) == 1" $
      \(a :: F) -> a /= 0 ==> a * fInv a == 1

  , testProperty "zero annihilation: a * 0 == 0" $
      \(a :: F) -> a * 0 == 0
  ]

-------------------------------------------------------------------------------
-- Distributivity
-------------------------------------------------------------------------------

distributivityTests :: TestTree
distributivityTests = testGroup "Distributivity"
  [ testProperty "left: a * (b + c) == a*b + a*c" $
      \(a :: F) b c -> a * (b + c) == a * b + a * c

  , testProperty "right: (a + b) * c == a*c + b*c" $
      \(a :: F) b c -> (a + b) * c == a * c + b * c
  ]

-------------------------------------------------------------------------------
-- Field-specific properties
-------------------------------------------------------------------------------

fieldSpecificTests :: TestTree
fieldSpecificTests = testGroup "Field properties"
  [ testProperty "division round-trip: b /= 0 ==> (a / b) * b == a" $
      \(a :: F) b -> b /= 0 ==> fDiv a b * b == a

  , testProperty "Fermat's little theorem: a /= 0 ==> a^(p-1) == 1" $
      \(a :: F) -> a /= 0 ==> a ^ (96 :: Int) == 1

  , testProperty "fromInteger round-trip: mkFp (unFp a) == a" $
      \(a :: F) -> mkFp (unFp a) == a

  , testProperty "unFp in range: 0 <= unFp a < p" $
      \(a :: F) -> let n = unFp a in n >= 0 && n < 97
  ]

-------------------------------------------------------------------------------
-- Utility function tests
-------------------------------------------------------------------------------

utilityTests :: TestTree
utilityTests = testGroup "Utilities"
  [ testProperty "extGcd: a*x + b*y == g" $
      \(Positive a) (Positive b) ->
        let (g, x, y) = extGcd a b
        in  a * x + b * y == g

  , testProperty "extGcd: g divides a and b" $
      \(Positive a) (Positive b) ->
        let (g, _, _) = extGcd a b
        in  a `mod` g == 0 && b `mod` g == 0

  , testProperty "modInverse: (a * modInverse a p) mod p == 1" $
      \(a :: F) -> a /= 0 ==>
        let n = unFp a
        in  (n * modInverse n 97) `mod` 97 == 1

  , testProperty "modPow matches (^): modPow (unFp a) e 97 == unFp (a ^ e)" $
      \(a :: F) (NonNegative e) ->
        let ei = (e :: Int) in
        modPow (unFp a) (toInteger ei) 97 == unFp (a ^ ei)
  ]

-------------------------------------------------------------------------------
-- Unit tests (hand-computed)
-------------------------------------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "Unit tests (F_97)"
  [ testCase "42 + 80 = 25" $
      (42 + 80 :: F) @?= 25

  , testCase "42 * 80 = 62" $
      (42 * 80 :: F) @?= mkFp (42 * 80)

  , testCase "negate 42 = 55" $
      negate (42 :: F) @?= 55

  , testCase "42 - 80 = 59" $
      (42 - 80 :: F) @?= 59

  , testCase "inv(42) = 67" $
      fInv (42 :: F) @?= 67

  , testCase "42 * inv(42) = 1" $
      (42 :: F) * fInv 42 @?= 1

  , testCase "0 is additive identity" $
      (0 :: F) + 42 @?= 42

  , testCase "1 is multiplicative identity" $
      (1 :: F) * 42 @?= 42

  , testCase "fromInteger 100 = 3" $
      (100 :: F) @?= 3

  , testCase "fromInteger (-1) = 96" $
      ((-1) :: F) @?= 96

  , testCase "extGcd 35 15 = (5, 1, -2)" $
      extGcd 35 15 @?= (5, 1, -2)

  , testCase "modInverse 42 97 = 67" $
      modInverse 42 97 @?= 67

  , testCase "modPow 3 10 97 = 73" $
      modPow 3 10 97 @?= 73
  ]

-------------------------------------------------------------------------------
-- Test on a second prime to catch hardcoded values
-------------------------------------------------------------------------------

type F2 = Fp 251

secondPrimeTests :: TestTree
secondPrimeTests = testGroup "F_251 (second prime)"
  [ testProperty "additive inverse" $
      \(a :: F2) -> a + negate a == 0

  , testProperty "multiplicative inverse" $
      \(a :: F2) -> a /= 0 ==> a * fInv a == 1

  , testProperty "distributivity" $
      \(a :: F2) b c -> a * (b + c) == a * b + a * c

  , testCase "200 + 100 = 49" $
      (200 + 100 :: F2) @?= 49

  , testCase "Fermat: a^250 = 1" $
      (7 :: F2) ^ (250 :: Int) @?= 1
  ]
