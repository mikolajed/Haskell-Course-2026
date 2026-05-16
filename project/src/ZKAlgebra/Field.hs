{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : ZKAlgebra.Field
-- Description : Prime field implementation
--
-- This module provides @Fp p@, a concrete implementation of a prime field
-- (integers mod p). The prime is encoded at the type level via @DataKinds@,
-- so @Fp 97@ and @Fp 251@ are /different types/.
--
-- @Fp p@ is an instance of @Num@, @Fractional@, and all the algebraic
-- typeclasses from "ZKAlgebra.Algebra".
module ZKAlgebra.Field
  ( -- * Re-exported algebraic hierarchy
    module ZKAlgebra.Algebra,

    -- * Prime field
    Fp (..),
    mkFp,
    unFp,
    fpPrime,

    -- * Utility functions
    extGcd,
    modInverse,
    modPow,
  )
where

import Data.Proxy (Proxy (..))
import Data.Ratio (denominator, numerator)
import GHC.TypeNats (KnownNat, Nat, natVal)
import ZKAlgebra.Algebra

-------------------------------------------------------------------------------
-- Prime field Fp
-------------------------------------------------------------------------------

-- | An element of the prime field F_p, represented as an integer in [0, p-1].
--
-- The prime @p@ lives at the type level:
--
-- >>> let a = 42 :: Fp 97
-- >>> let b = 80 :: Fp 97
-- >>> a + b
-- 25
--
-- Trying to add elements from different fields is a type error:
-- >>> (1 :: Fp 97) + (1 :: Fp 251)   -- won't compile!
-- Couldn't match type `251' with `97'
-- Expected: Fp 97
--   Actual: Fp 251
-- In the second argument of `(+)', namely `(1 :: Fp 251)'
-- In the expression: (1 :: Fp 97) + (1 :: Fp 251)
-- In an equation for `it_aexi':
--     it_aexi = (1 :: Fp 97) + (1 :: Fp 251)
newtype Fp (p :: Nat) = Fp Integer
  deriving (Eq, Ord)

-- | Smart constructor: reduces the input modulo @p@.
--
-- >>> mkFp @97 100
-- 3
mkFp :: forall p. (KnownNat p) => Integer -> Fp p
mkFp n = Fp (n `mod` toInteger (natVal (Proxy :: Proxy p)))

-- | Extract the underlying integer in [0, p-1].
unFp :: Fp p -> Integer
unFp (Fp n) = n

-- | Retrieve the prime modulus from a field element's type.
--
-- >>> fpPrime (42 :: Fp 97)
-- 97
fpPrime :: forall p. (KnownNat p) => Fp p -> Integer
fpPrime _ = toInteger (natVal (Proxy :: Proxy p))

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance (KnownNat p) => Show (Fp p) where
  show (Fp n) = show n

instance (KnownNat p) => Num (Fp p) where
  (Fp a) + (Fp b) = mkFp (a + b)
  (Fp a) * (Fp b) = mkFp (a * b)
  (Fp a) - (Fp b) = mkFp (a - b)
  negate (Fp 0) = Fp 0
  negate (Fp a) = Fp (toInteger (natVal (Proxy :: Proxy p)) - a)
  abs = id
  signum (Fp 0) = Fp 0
  signum _ = Fp 1
  fromInteger = mkFp

instance (KnownNat p) => Fractional (Fp p) where
  recip = fInv
  (/) = fDiv
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance (KnownNat p) => Ring (Fp p)

instance (KnownNat p) => Field (Fp p) where
  fInv (Fp 0) = error "ZKAlgebra.Field.fInv: inverse of zero"
  fInv (Fp a) = mkFp (modInverse a (toInteger (natVal (Proxy :: Proxy p))))

instance (KnownNat p) => FiniteField (Fp p) where
  fieldOrder _ = toInteger (natVal (Proxy :: Proxy p))

-------------------------------------------------------------------------------
-- Number-theoretic utilities
-------------------------------------------------------------------------------

-- | Extended Euclidean algorithm.
--
-- Given @a@ and @b@, returns @(g, x, y)@ such that @a*x + b*y = g@
-- where @g = gcd(a, b)@.
--
-- >>> extGcd 35 15
-- (5,1,-2)
extGcd :: Integer -> Integer -> (Integer, Integer, Integer)
extGcd 0 b = (b, 0, 1)
extGcd a b =
  let (g, x, y) = extGcd (b `mod` a) a
   in (g, y - (b `div` a) * x, x)

-- | Modular inverse: the unique @x@ such that @a * x ≡ 1 (mod m)@.
--
-- Requires @gcd(a, m) = 1@; errors otherwise.
--
-- >>> modInverse 42 97
-- 67
modInverse :: Integer -> Integer -> Integer
modInverse a m =
  let (g, x, _) = extGcd (a `mod` m) m
   in if g /= 1
        then
          error $
            "ZKAlgebra.Field.modInverse: "
              ++ show a
              ++ " has no inverse mod "
              ++ show m
        else x `mod` m

-- | Fast modular exponentiation by repeated squaring.
--
-- @modPow base exp modulus@ computes @base^exp mod modulus@.
-- Handles negative exponents via modular inverse.
--
-- >>> modPow 3 10 97
-- 73
modPow :: Integer -> Integer -> Integer -> Integer
modPow _ _ 1 = 0
modPow base e modulus
  | e < 0 = modPow (modInverse base modulus) (negate e) modulus
  | otherwise = go (base `mod` modulus) e 1
  where
    go _ 0 acc = acc
    go b n acc
      | odd n = go b' n' ((acc * b) `mod` modulus)
      | otherwise = go b' n' acc
      where
        b' = (b * b) `mod` modulus
        n' = n `div` 2
