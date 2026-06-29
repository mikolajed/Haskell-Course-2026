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
-- We implement MiMC-5 (using x^5). For the permutation to be invertible,
-- the exponent (5) must be coprime with p-1. This is true for F_97 and F_251.
module ZKAlgebra.Crypto.MiMC
  ( -- * Constants
    mimcConstants,

    -- * Round Function
    mimc5Round,

    -- * Hashing
    mimcHash,
  )
where

import ZKAlgebra.Field

-- | Generate 110 round constants deterministically.
-- In a production deployment, these are typically derived by repeatedly
-- hashing a seed (like "MiMC") using Keccak256. For this educational
-- implementation, we use a simple deterministic polynomial generator.
mimcConstants :: Field f => [f]
mimcConstants = [fromInteger (i * i * 17 + 3) | i <- [1 .. 110]]

-- | A single round of the MiMC-5 block cipher.
--
-- Formula: @x_next = (x + k + c_i)^5@
--
-- Note that computing x^5 natively in a ZK circuit requires only 3
-- multiplications:
--   t2 = t * t
--   t4 = t2 * t2
--   t5 = t4 * t
mimc5Round :: Field f => f -> f -> f -> f
mimc5Round x k c =
  let t = x + k + c
      t2 = t * t
      t4 = t2 * t2
   in t4 * t

-- | Hash a single field element using the MiMC-5 block cipher.
--
-- This uses a variant of the Miyaguchi-Preneel compression mode:
-- @Hash(x) = E_x(0) + x@
-- where @E_k(m)@ is the block cipher encrypting message @m@ with key @k@.
-- Here we encrypt 0 using our input @x@ as the key.
mimcHash :: Field f => f -> f
mimcHash x =
  let -- Start with state = 0.
      -- Fold the round function over the 110 round constants.
      finalState = foldl (\state c_i -> mimc5Round state x c_i) 0 mimcConstants
   in -- Add the input to the final state (Miyaguchi-Preneel feed-forward)
      finalState + x
