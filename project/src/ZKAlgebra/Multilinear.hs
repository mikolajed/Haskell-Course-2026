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
    go values (r : rs) = go (V.fromList [interpolatePair p r | p <- activePairs values]) rs

-- | Fix the first variable to a field element.
mleFix :: (Field f) => MultilinearPoly f -> f -> MultilinearPoly f
mleFix (MLP numVars evals) r
  | numVars <= 0 =
      error "ZKAlgebra.Multilinear.mleFix: cannot fix a zero-variable polynomial"
  | otherwise = MLP (numVars - 1) (V.fromList [interpolatePair p r | p <- activePairs evals])

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
    g0 = sum [ v0 | (v0, _) <- activePairs evals ]
    g1 = sum [ v1 | (_, v1) <- activePairs evals ]

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
    pairsA = activePairs evalsA
    pairsB = activePairs evalsB
    sumAt t =
      sum
        [ interpolatePair pA t * interpolatePair pB t
        | (pA, pB) <- zip pairsA pairsB
        ]
    g0 = sumAt 0
    g1 = sumAt 1
    g2 = sumAt 2

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Extracts the pairs of evaluations corresponding to the active variable (x1).
-- Each pair contains the evaluation at x1=0 and x1=1 for a specific configuration
-- of the remaining variables.
activePairs :: Vector f -> [(f, f)]
activePairs values =
  [ (values V.! (2 * i), values V.! (2 * i + 1))
  | i <- [0 .. (V.length values `div` 2) - 1]
  ]

-- | Linearly interpolate between two evaluations: v0 at t=0, and v1 at t=1.
interpolatePair :: (Field f) => (f, f) -> f -> f
interpolatePair (v0, v1) t = v0 * (1 - t) + v1 * t

hypercubeSize :: Int -> Int
hypercubeSize numVars = 2 ^ numVars
