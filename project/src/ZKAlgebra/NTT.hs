{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : ZKAlgebra.NTT
-- Description : Number-Theoretic Transform over prime fields
--
-- The Number-Theoretic Transform is the finite-field analogue of the FFT.
-- Given a primitive @n@-th root of unity @omega@, it evaluates a polynomial
-- with @n@ coefficients at:
--
-- @
--   1, omega, omega^2, ..., omega^(n-1)
-- @
--
-- This module uses that transform to implement fast polynomial multiplication
-- over prime fields.
module ZKAlgebra.NTT
  ( -- * Roots of unity
    primitiveRoot,

    -- * Transform
    ntt,
    invNtt,

    -- * Polynomial multiplication
    polyMulNTT,
  )
where

import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, natVal)
import ZKAlgebra.Algebra
import ZKAlgebra.Field
import ZKAlgebra.Polynomial

-------------------------------------------------------------------------------
-- Roots of unity
-------------------------------------------------------------------------------

-- | Find a primitive @n@-th root of unity in @F_p@.
--
-- Requires @n > 0@ and @n@ to divide @p - 1@. For @n = 1@, the root is @1@.
primitiveRoot :: forall p. (KnownNat p) => Int -> Fp p
primitiveRoot n
  | n <= 0 =
      error "ZKAlgebra.NTT.primitiveRoot: order must be positive"
  | n == 1 = 1
  | groupOrder `mod` nInteger /= 0 =
      error "ZKAlgebra.NTT.primitiveRoot: order does not divide p - 1"
  | otherwise = generator ^ (groupOrder `div` nInteger)
  where
    modulus = toInteger (natVal (Proxy :: Proxy p))
    groupOrder = modulus - 1
    nInteger = toInteger n
    factors = primeFactors groupOrder

    generator =
      case [g | a <- [2 .. modulus - 1], let g = mkFp @p a, isGenerator g] of
        (g : _) -> g
        [] -> error "ZKAlgebra.NTT.primitiveRoot: no field generator found"

    isGenerator g =
      all (\q -> g ^ (groupOrder `div` q) /= 1) factors

-------------------------------------------------------------------------------
-- Transform
-------------------------------------------------------------------------------

-- | Forward Number-Theoretic Transform.
--
-- The input length must be a power of two. The supplied root must be a
-- primitive root of unity of that same order.
ntt :: (Field f) => f -> [f] -> [f]
ntt _ [] = []
ntt root xs
  | not (isPowerOfTwo (length xs)) =
      error "ZKAlgebra.NTT.ntt: input length must be a power of two"
  | otherwise = nttPowerOfTwo root xs

-- | Inverse Number-Theoretic Transform.
--
-- The root should be the same primitive root used for the forward transform.
invNtt :: (Field f) => f -> [f] -> [f]
invNtt _ [] = []
invNtt root xs =
  let nInv = fInv (fromInteger (toInteger (length xs)))
   in map (nInv *) (ntt (fInv root) xs)

nttPowerOfTwo :: (Field f) => f -> [f] -> [f]
nttPowerOfTwo _ [x] = [x]
nttPowerOfTwo root xs =
  let rootSquared = root * root
      evens = nttPowerOfTwo rootSquared (evenIndexed xs)
      odds = nttPowerOfTwo rootSquared (oddIndexed xs)
      half = length xs `div` 2

      -- Cooley-Tukey butterfly: A(x) = A_even(x^2) + x*A_odd(x^2).
      -- Each pair combines as even +/- root^k * odd.
      twiddles = take half (iterate (* root) 1)
      oddTerms = zipWith (*) twiddles odds
   in zipWith (+) evens oddTerms ++ zipWith (-) evens oddTerms

-------------------------------------------------------------------------------
-- Polynomial multiplication
-------------------------------------------------------------------------------

-- | Fast polynomial multiplication using the NTT.
--
-- Coefficients are padded to the next power of two, transformed, multiplied
-- pointwise, and transformed back.
polyMulNTT :: forall p. (KnownNat p) => Poly (Fp p) -> Poly (Fp p) -> Poly (Fp p)
polyMulNTT (Poly []) _ = Poly []
polyMulNTT _ (Poly []) = Poly []
polyMulNTT (Poly as) (Poly bs) =
  poly (take productLength coefficients)
  where
    productLength = length as + length bs - 1
    size = nextPowerOfTwo productLength
    root = primitiveRoot @p size
    pad xs = xs ++ replicate (size - length xs) 0
    asEval = ntt root (pad as)
    bsEval = ntt root (pad bs)
    productEval = zipWith (*) asEval bsEval
    coefficients = invNtt root productEval

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n
  | n <= 0 = False
  | n == 1 = True
  | even n = isPowerOfTwo (n `div` 2)
  | otherwise = False

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n
  | n <= 1 = 1
  | otherwise = go 1
  where
    go size
      | size >= n = size
      | otherwise = go (size * 2)

evenIndexed :: [a] -> [a]
evenIndexed [] = []
evenIndexed (x : xs) = x : oddIndexed xs

oddIndexed :: [a] -> [a]
oddIndexed [] = []
oddIndexed (_ : xs) = evenIndexed xs

primeFactors :: Integer -> [Integer]
primeFactors n
  | n < 2 = []
  | otherwise = go n 2 []
  where
    go m divisor factors
      | divisor * divisor > m =
          reverse (if m == 1 then factors else m : factors)
      | m `mod` divisor == 0 =
          go (divideOut m divisor) (nextDivisor divisor) (divisor : factors)
      | otherwise =
          go m (nextDivisor divisor) factors

    divideOut m divisor
      | m `mod` divisor == 0 = divideOut (m `div` divisor) divisor
      | otherwise = m

    nextDivisor 2 = 3
    nextDivisor divisor = divisor + 2
