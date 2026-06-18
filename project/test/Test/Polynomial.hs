{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Property-based and unit tests for "ZKAlgebra.Polynomial".
--
-- The properties check the expected algebraic identities for polynomial
-- evaluation, multiplication, division, and interpolation over F_97.

module Test.Polynomial (polynomialTests) where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import ZKAlgebra.Field
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

genPoly :: Int -> Gen (Poly F)
genPoly maxDegree = do
  len <- chooseInt (0, maxDegree + 1)
  poly <$> vectorOf len genF

genNonZeroPoly :: Int -> Gen (Poly F)
genNonZeroPoly maxDegree = genPoly maxDegree `suchThat` (/= Poly [])

genDistinctPoints :: Gen [(F, F)]
genDistinctPoints = do
  n <- chooseInt (1, 6)
  xs <- take n <$> shuffle [0 .. 20]
  ys <- vectorOf n genF
  pure [(mkFp x, y) | (x, y) <- zip xs ys]

-------------------------------------------------------------------------------
-- Test tree
-------------------------------------------------------------------------------

polynomialTests :: TestTree
polynomialTests =
  testGroup
    "ZKAlgebra.Polynomial"
    [ constructionTests,
      evaluationTests,
      arithmeticTests,
      divisionTests,
      interpolationTests,
      unitTests
    ]

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

constructionTests :: TestTree
constructionTests =
  testGroup
    "Construction"
    [ testProperty "normalization strips trailing zeros" $
        forAll (listOf genF) $ \cs ->
          let p = poly (cs ++ [0, 0, 0])
           in polyCoeffs p == polyCoeffs (poly cs),
      testProperty "polyConst agrees with poly singleton" $
        forAll genF $ \a ->
          polyConst a == poly [a],
      testCase "zero polynomial has degree -1" $
        degree (poly [] :: Poly F) @?= -1,
      testCase "degree of 3 + x + x^3 is 3" $
        degree (poly [3, 1, 0, 1] :: Poly F) @?= 3
    ]

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

evaluationTests :: TestTree
evaluationTests =
  testGroup
    "Evaluation"
    [ testProperty "eval(p + q) == eval(p) + eval(q)" $
        forAll (genPoly 8) $ \p ->
          forAll (genPoly 8) $ \q ->
            forAll genF $ \x ->
              polyEval (p + q) x == polyEval p x + polyEval q x,
      testProperty "eval(p * q) == eval(p) * eval(q)" $
        forAll (genPoly 6) $ \p ->
          forAll (genPoly 6) $ \q ->
            forAll genF $ \x ->
              polyEval (p * q) x == polyEval p x * polyEval q x,
      testProperty "eval constant polynomial" $
        forAll genF $ \a ->
          forAll genF $ \x ->
            polyEval (polyConst a) x == a
    ]

-------------------------------------------------------------------------------
-- Arithmetic
-------------------------------------------------------------------------------

arithmeticTests :: TestTree
arithmeticTests =
  testGroup
    "Arithmetic"
    [ testProperty "addition is commutative" $
        forAll (genPoly 8) $ \p ->
          forAll (genPoly 8) $ \q ->
            p + q == q + p,
      testProperty "addition is associative" $
        forAll (genPoly 6) $ \p ->
          forAll (genPoly 6) $ \q ->
            forAll (genPoly 6) $ \r ->
              (p + q) + r == p + (q + r),
      testProperty "multiplication is commutative" $
        forAll (genPoly 6) $ \p ->
          forAll (genPoly 6) $ \q ->
            p * q == q * p,
      testProperty "multiplication is associative" $
        forAll (genPoly 4) $ \p ->
          forAll (genPoly 4) $ \q ->
            forAll (genPoly 4) $ \r ->
              (p * q) * r == p * (q * r),
      testProperty "distributivity" $
        forAll (genPoly 5) $ \p ->
          forAll (genPoly 5) $ \q ->
            forAll (genPoly 5) $ \r ->
              p * (q + r) == p * q + p * r,
      testProperty "degree(p*q) == degree p + degree q for nonzero p,q" $
        forAll (genNonZeroPoly 6) $ \p ->
          forAll (genNonZeroPoly 6) $ \q ->
            degree (p * q) == degree p + degree q
    ]

-------------------------------------------------------------------------------
-- Division
-------------------------------------------------------------------------------

divisionTests :: TestTree
divisionTests =
  testGroup
    "Division"
    [ testProperty "p == q*d + r" $
        forAll (genPoly 8) $ \p ->
          forAll (genNonZeroPoly 5) $ \d ->
            let (q, r) = polyDiv p d
             in p == q * d + r,
      testProperty "remainder degree is smaller than divisor degree" $
        forAll (genPoly 8) $ \p ->
          forAll (genNonZeroPoly 5) $ \d ->
            let (_, r) = polyDiv p d
             in r == Poly [] || degree r < degree d
    ]

-------------------------------------------------------------------------------
-- Interpolation
-------------------------------------------------------------------------------

interpolationTests :: TestTree
interpolationTests =
  testGroup
    "Interpolation"
    [ testProperty "lagrange polynomial evaluates to each input point" $
        forAll genDistinctPoints $ \pts ->
          let p = lagrange pts
           in all (\(x, y) -> polyEval p x == y) pts,
      testProperty "interpolating samples from a polynomial recovers it" $
        forAll (genPoly 5) $ \p ->
          let d = degree p
              xs = [0 .. max 0 d]
              pts =
                [ (mkFp (toInteger x), polyEval p (mkFp (toInteger x)))
                  | x <- xs
                ]
           in lagrange pts == p
    ]

-------------------------------------------------------------------------------
-- Unit tests (hand-computed)
-------------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests (F_97)"
    [ testCase "poly strips trailing zeros" $
        (poly [1, 2, 0, 0] :: Poly F) @?= Poly [1, 2],
      testCase "polyEval (3 + x + x^3) 5 = 36" $
        polyEval (poly [3, 1, 0, 1] :: Poly F) 5 @?= 36,
      testCase "(3 + x + x^3) + (1 + x) = 4 + 2x + x^3" $
        (poly [3, 1, 0, 1] :: Poly F) + poly [1, 1]
          @?= poly [4, 2, 0, 1],
      testCase "(3 + x + x^3)(1 + x) = 3 + 4x + x^2 + x^3 + x^4" $
        (poly [3, 1, 0, 1] :: Poly F) * poly [1, 1]
          @?= poly [3, 4, 1, 1, 1],
      testCase "polynomial division recovers quotient and zero remainder" $
        polyDiv (poly [3, 4, 1, 1, 1] :: Poly F) (poly [1, 1])
          @?= (poly [3, 1, 0, 1], Poly []),
      testCase "division by higher-degree polynomial returns zero quotient" $
        polyDiv (poly [1, 2] :: Poly F) (poly [1, 0, 1])
          @?= (Poly [], poly [1, 2]),
      testCase "lagrange interpolation recovers 3 + x + x^3" $
        lagrange
          [ (0, 3),
            (1, 5),
            (2, 13),
            (3, 33)
          ]
          @?= (poly [3, 1, 0, 1] :: Poly F),
      testCase "Num instance supports fromInteger and multiplication" $
        (poly [1, 1] :: Poly F) * 3 + 4 @?= poly [7, 3]
    ]
