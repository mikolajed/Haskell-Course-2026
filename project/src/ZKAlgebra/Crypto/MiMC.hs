{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : ZKAlgebra.Crypto.MiMC
-- Description : MiMC Algebraic Hash Function
--
-- MiMC (Minimal Multiplicative Complexity) is a zero-knowledge friendly
-- hash function. It operates directly on finite field elements rather than
-- bits, making it extremely efficient to prove inside a SNARK (e.g., using
-- hundreds of constraints instead of tens of thousands like SHA-256).
--
-- We implement a dynamic MiMC. For the permutation to be invertible,
-- the exponent (d) must be coprime with p-1. We dynamically select the
-- lowest secure exponent d in {3, 5, 7, 11} based on the field characteristic.
module ZKAlgebra.Crypto.MiMC
  ( -- * Constants
    mimcConstants,

    -- * Round Exponent
    mimcExponent,

    -- * Hashing
    mimcHash,
  )
where

import Data.Proxy (Proxy (..))
import ZKAlgebra.Algebra
import ZKAlgebra.Field

-- | Generate 110 round constants deterministically.
-- In a production deployment, these are typically derived by repeatedly
-- hashing a seed (like "MiMC") using Keccak256. For this educational
-- implementation, we use a simple deterministic polynomial generator.
mimcConstants :: Field f => [f]
mimcConstants = [fromInteger (i * i * 17 + 3) | i <- [1 .. 110]]

-- | Dynamically select the lowest secure exponent for the MiMC block cipher.
-- For f(x) = x^d to be a permutation over F_p, we must have gcd(d, p-1) == 1.
mimcExponent :: Integer -> Integer
mimcExponent p
  | gcd 3 (p - 1) == 1 = 3
  | gcd 5 (p - 1) == 1 = 5
  | gcd 7 (p - 1) == 1 = 7
  | gcd 11 (p - 1) == 1 = 11
  | otherwise = error "Could not find a small MiMC exponent for this prime"

-- | Hash a single field element using the MiMC block cipher.
--
-- This uses a variant of the Miyaguchi-Preneel compression mode:
-- @Hash(x) = E_x(0) + x@
-- where @E_k(m)@ is the block cipher encrypting message @m@ with key @k@.
-- Here we encrypt 0 using our input @x@ as the key.
mimcHash :: forall f. FiniteField f => f -> f
mimcHash x =
  let p = fieldOrder (Proxy :: Proxy f)
      d = mimcExponent p
      
      -- A single round of the MiMC block cipher: x_next = (state + k + c_i)^d
      mimcRound state c_i = (state + x + c_i) ^ d
      
      -- Start with state = 0.
      -- Fold the round function over the 110 round constants.
      finalState = foldl mimcRound 0 mimcConstants
   in -- Add the input to the final state (Miyaguchi-Preneel feed-forward)
      finalState + x
