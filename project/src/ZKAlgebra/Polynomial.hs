-- |
-- Module      : ZKAlgebra.Polynomial
-- Description : Univariate polynomials over fields
--
-- This module provides dense, coefficient-form univariate polynomials.
-- Coefficients are stored in ascending degree order:
--
-- @
--   Poly [3, 1, 0, 1] = 3 + x + x^3
-- @
--
-- The zero polynomial is represented as @Poly []@. Values constructed with
-- 'poly' and all operations in this module are normalized: trailing zero
-- coefficients are stripped.
module ZKAlgebra.Polynomial
  ( -- * Polynomial type
    Poly (..),

    -- * Construction
    poly,
    polyConst,
    polyX,
    degree,

    -- * Evaluation
    polyEval,

    -- * Arithmetic
    polyAdd,
    polyNeg,
    polySub,
    polyMul,
    polyScale,

    -- * Division
    polyDiv,

    -- * Interpolation
    lagrange,
  )
where

import ZKAlgebra.Algebra

-------------------------------------------------------------------------------
-- Polynomial type
-------------------------------------------------------------------------------

-- | A univariate polynomial over a coefficient field @f@.
--
-- Coefficients are stored from constant term upward. The invariant is that no
-- polynomial has trailing zero coefficients; use 'poly' to construct values
-- while preserving that invariant.
newtype Poly f = Poly {polyCoeffs :: [f]}
  deriving (Eq)

instance (Show f) => Show (Poly f) where
  show (Poly cs) = "Poly " ++ show cs

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Construct a normalized polynomial by stripping trailing zeros.
poly :: (Field f) => [f] -> Poly f
poly = Poly . stripTrailingZeros

-- | Construct a constant polynomial.
polyConst :: (Field f) => f -> Poly f
polyConst c = poly [c]

-- | The polynomial @x@.
polyX :: (Field f) => Poly f
polyX = Poly [0, 1]

-- | Polynomial degree. The zero polynomial has degree @-1@.
degree :: Poly f -> Int
degree (Poly []) = -1
degree (Poly cs) = length cs - 1

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

-- | Evaluate a polynomial at a point using Horner's method.
polyEval :: (Field f) => Poly f -> f -> f
polyEval (Poly cs) x = foldr (\c acc -> c + x * acc) 0 cs

-------------------------------------------------------------------------------
-- Arithmetic
-------------------------------------------------------------------------------

-- | Add two polynomials.
polyAdd :: (Field f) => Poly f -> Poly f -> Poly f
polyAdd (Poly as) (Poly bs) = poly (addCoeffs as bs)

-- | Negate a polynomial.
polyNeg :: (Field f) => Poly f -> Poly f
polyNeg (Poly cs) = poly (map negate cs)

-- | Subtract two polynomials.
polySub :: (Field f) => Poly f -> Poly f -> Poly f
polySub p q = polyAdd p (polyNeg q)

-- | Multiply two polynomials using the naive O(n^2) convolution.
polyMul :: (Field f) => Poly f -> Poly f -> Poly f
polyMul (Poly as) (Poly bs) = poly (mulCoeffs as bs)

-- | Multiply every coefficient by a scalar.
polyScale :: (Field f) => f -> Poly f -> Poly f
polyScale c (Poly cs) = poly (map (c *) cs)

instance (Field f) => Num (Poly f) where
  (+) = polyAdd
  (-) = polySub
  (*) = polyMul
  negate = polyNeg
  fromInteger n = polyConst (fromInteger n)
  abs = id
  signum (Poly []) = 0
  signum _ = 1

-------------------------------------------------------------------------------
-- Division
-------------------------------------------------------------------------------

-- | Polynomial long division.
--
-- @polyDiv dividend divisor@ returns @(quotient, remainder)@ such that:
--
-- @
--   dividend = quotient * divisor + remainder
-- @
--
-- The divisor must be nonzero.
polyDiv :: (Field f) => Poly f -> Poly f -> (Poly f, Poly f)
polyDiv dividend divisor
  | divisor' == Poly [] =
      error "ZKAlgebra.Polynomial.polyDiv: division by zero polynomial"
  | degree dividend' < degree divisor' = (Poly [], dividend')
  | otherwise = go dividend' (Poly [])
  where
    dividend' = normalize dividend
    divisor' = normalize divisor
    divisorDegree = degree divisor'
    divisorLead = leadingCoeff divisor'

    go remainder quotient
      | degree remainder < divisorDegree = (quotient, remainder)
      | otherwise =
          let coeff = fDiv (leadingCoeff remainder) divisorLead
              shift = degree remainder - divisorDegree
              term = Poly (replicate shift 0 ++ [coeff])
              remainder' = polySub remainder (polyMul term divisor')
              quotient' = polyAdd quotient term
           in go remainder' quotient'

-------------------------------------------------------------------------------
-- Interpolation
-------------------------------------------------------------------------------

-- | Interpolate the unique polynomial passing through the given points.
--
-- The x-coordinates must be distinct. An empty point set interpolates to the
-- zero polynomial.
lagrange :: (Field f) => [(f, f)] -> Poly f
lagrange pts
  | hasDuplicates (map fst pts) =
      error "ZKAlgebra.Polynomial.lagrange: duplicate x-coordinates"
  | otherwise =
      foldr polyAdd (Poly []) (map basis indexed)
  where
    indexed = zip [(0 :: Int) ..] pts

    basis (i, (xi, yi)) =
      polyScale yi $
        foldr polyMul (polyConst 1)
          [ polyScale (fDiv 1 (xi - xj)) (poly [-xj, 1])
            | (j, (xj, _)) <- indexed,
              i /= j
          ]

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

normalize :: (Field f) => Poly f -> Poly f
normalize (Poly cs) = poly cs

stripTrailingZeros :: (Eq f, Num f) => [f] -> [f]
stripTrailingZeros = reverse . dropWhile (== 0) . reverse

addCoeffs :: (Num f) => [f] -> [f] -> [f]
addCoeffs [] bs = bs
addCoeffs as [] = as
addCoeffs (a : as) (b : bs) = (a + b) : addCoeffs as bs

mulCoeffs :: (Num f) => [f] -> [f] -> [f]
mulCoeffs [] _ = []
mulCoeffs _ [] = []
mulCoeffs (a : as) bs =
  addCoeffs (map (a *) bs) (0 : mulCoeffs as bs)

leadingCoeff :: Poly f -> f
leadingCoeff (Poly []) =
  error "ZKAlgebra.Polynomial.leadingCoeff: zero polynomial"
leadingCoeff (Poly cs) = last cs

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x : xs) = x `elem` xs || hasDuplicates xs
