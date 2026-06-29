-- |
-- Module      : ZKAlgebra.Multilinear
-- Description : Multilinear polynomials on the boolean hypercube
--
-- A multilinear polynomial in @n@ variables is represented by its @2^n@
-- evaluations on the boolean hypercube @{0,1}^n@.
--
-- The evaluation table uses little-endian variable order: bit 0 of the index is
-- @x1@, bit 1 is @x2@, and so on. For two variables, the order is:
--
-- @
--   f(0,0), f(1,0), f(0,1), f(1,1)
-- @
module ZKAlgebra.Multilinear
  ( -- * Multilinear polynomial type
    MultilinearPoly (..),

    -- * Construction
    mlp,

    -- * Evaluation and fixing
    mleEval,
    mleFix,

    -- * Sums
    mleSum,
    mlePartialSum,
    mlePartialSumProduct,
  )
where

import Data.Vector (Vector)
import qualified Data.Vector as V
import ZKAlgebra.Algebra
import ZKAlgebra.Polynomial

-------------------------------------------------------------------------------
-- Multilinear polynomial type
-------------------------------------------------------------------------------

-- | A multilinear polynomial @f : F^n -> F@, stored as evaluations on
-- @{0,1}^n@.
data MultilinearPoly f = MLP
  { mlpNumVars :: !Int,
    mlpEvals :: !(Vector f)
  }
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Construct a multilinear polynomial from its boolean-hypercube evaluations.
--
-- The number of evaluations must be exactly @2^numVars@.
mlp :: (Field f) => Int -> [f] -> MultilinearPoly f
mlp numVars evals
  | numVars < 0 =
      error "ZKAlgebra.Multilinear.mlp: number of variables must be nonnegative"
  | length evals /= expectedSize =
      error "ZKAlgebra.Multilinear.mlp: expected exactly 2^numVars evaluations"
  | otherwise = MLP numVars (V.fromList evals)
  where
    expectedSize = hypercubeSize numVars

-------------------------------------------------------------------------------
-- Evaluation and fixing
-------------------------------------------------------------------------------

-- | Evaluate the multilinear extension at an arbitrary point in @F^n@.
mleEval :: (Field f) => MultilinearPoly f -> [f] -> f
mleEval (MLP numVars evals) point
  | length point /= numVars =
      error "ZKAlgebra.Multilinear.mleEval: point dimension mismatch"
  | otherwise = go evals point
  where
    go values [] = V.head values
    go values (r : rs) = go (fixFirstVariable values r) rs

-- | Fix the first variable to a field element.
mleFix :: (Field f) => MultilinearPoly f -> f -> MultilinearPoly f
mleFix (MLP numVars evals) r
  | numVars <= 0 =
      error "ZKAlgebra.Multilinear.mleFix: cannot fix a zero-variable polynomial"
  | otherwise = MLP (numVars - 1) (fixFirstVariable evals r)

-------------------------------------------------------------------------------
-- Sums
-------------------------------------------------------------------------------

-- | Sum all evaluations over the boolean hypercube.
mleSum :: (Field f) => MultilinearPoly f -> f
mleSum (MLP _ evals) = V.foldl' (+) 0 evals

-- | Sum over all variables except the first, producing a univariate polynomial.
--
-- For @g(X) = sum f(X, x2, ..., xn)@:
--
-- @
--   g(0) = sum of entries with x1 = 0
--   g(1) = sum of entries with x1 = 1
-- @
mlePartialSum :: (Field f) => MultilinearPoly f -> Poly f
mlePartialSum (MLP numVars evals)
  | numVars <= 0 =
      error "ZKAlgebra.Multilinear.mlePartialSum: needs at least one variable"
  | otherwise = poly [g0, g1 - g0]
  where
    g0 = sumFirstBit 0 evals
    g1 = sumFirstBit 1 evals

-- | Partial sum of the product of two multilinear polynomials.
--
-- Given @a@ and @b@ (both @n@-variate), computes:
--
-- @
--   g(X) = Σ_{x₂,...,xₙ ∈ {0,1}} a(X, x₂, …, xₙ) · b(X, x₂, …, xₙ)
-- @
--
-- Since @a@ and @b@ are each linear in @X@, their product is quadratic,
-- so @g@ has degree at most 2. We evaluate at three points and interpolate.
mlePartialSumProduct ::
  (Field f) => MultilinearPoly f -> MultilinearPoly f -> Poly f
mlePartialSumProduct (MLP numVarsA evalsA) (MLP numVarsB evalsB)
  | numVarsA <= 0 || numVarsB <= 0 =
      error "ZKAlgebra.Multilinear.mlePartialSumProduct: needs at least one variable"
  | numVarsA /= numVarsB =
      error "ZKAlgebra.Multilinear.mlePartialSumProduct: variable count mismatch"
  | otherwise = lagrange [(0, g0), (1, g1), (2, g2)]
  where
    halfSize = V.length evalsA `div` 2
    sumAt t =
      sum
        [ let a0 = evalsA V.! (2 * i)
              a1 = evalsA V.! (2 * i + 1)
              b0 = evalsB V.! (2 * i)
              b1 = evalsB V.! (2 * i + 1)
              aVal = a0 * (1 - t) + a1 * t
              bVal = b0 * (1 - t) + b1 * t
           in aVal * bVal
          | i <- [0 .. halfSize - 1]
        ]
    g0 = sumAt 0
    g1 = sumAt 1
    g2 = sumAt 2

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

fixFirstVariable :: (Field f) => Vector f -> f -> Vector f
fixFirstVariable values r =
  V.generate newSize $ \i ->
    let v0 = values V.! (2 * i)
        v1 = values V.! (2 * i + 1)
     in v0 * (1 - r) + v1 * r
  where
    newSize = V.length values `div` 2

sumFirstBit :: (Field f) => Int -> Vector f -> f
sumFirstBit bit =
  V.ifoldl' (\acc i value -> if i `mod` 2 == bit then acc + value else acc) 0

hypercubeSize :: Int -> Int
hypercubeSize numVars = 2 ^ numVars
