{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : ZKAlgebra.Crypto.FiatShamir
-- Description : Fiat-Shamir transform for non-interactive sumcheck
--
-- Turns the interactive sumcheck protocol into a non-interactive one by
-- deriving verifier challenges from a hash of the transcript so far.
--
-- This module uses the ZK-friendly MiMC algebraic hash function, allowing
-- the transcript to operate entirely on field elements without any bitwise
-- serialization.
module ZKAlgebra.Crypto.FiatShamir
  ( -- * Transcript
    Transcript (..),
    emptyTranscript,

    -- * Transcript operations
    appendToTranscript,
    challenge,

    -- * Non-interactive sumcheck
    sumcheckProveFS,
    sumcheckProveFSProduct,
    sumcheckVerifyFS,
  )
where

import Control.Monad.State.Strict (State, get, modify', evalState, runState)
import GHC.TypeNats (KnownNat)
import ZKAlgebra.Crypto.MiMC (mimcHash)
import ZKAlgebra.Crypto.Sumcheck
import ZKAlgebra.Field
import ZKAlgebra.Multilinear
import ZKAlgebra.Polynomial

-------------------------------------------------------------------------------
-- Transcript
-------------------------------------------------------------------------------

-- | An append-only transcript represented purely as a running field element state.
newtype Transcript f = Transcript f
  deriving (Eq, Show)

-- | An empty transcript initializes to 0.
emptyTranscript :: FiniteField f => Transcript f
emptyTranscript = Transcript 0

-------------------------------------------------------------------------------
-- Transcript operations (State monad)
-------------------------------------------------------------------------------

-- | Append field elements to the transcript by absorbing them with MiMC.
appendToTranscript :: FiniteField f => [f] -> State (Transcript f) ()
appendToTranscript vals = modify' $ \(Transcript state) ->
  let newState = foldl (\s v -> mimcHash (s + v)) state vals
   in Transcript newState

-- | Derive a challenge field element from the current transcript state.
--
-- We simply hash the state one more time to get the challenge, and store
-- that hash as the new state (acting as a domain separator for the next challenge).
challenge :: FiniteField f => State (Transcript f) f
challenge = do
  Transcript state <- get
  let digest = mimcHash state
  -- The digest itself becomes the new transcript state
  modify' $ const (Transcript digest)
  return digest

-------------------------------------------------------------------------------
-- Non-interactive sumcheck
-------------------------------------------------------------------------------

-- | Non-interactive sumcheck prover using the Fiat-Shamir heuristic.
sumcheckProveFS ::
  forall p.
  (KnownNat p) =>
  MultilinearPoly (Fp p) ->
  Fp p ->
  SumcheckProof (Fp p)
sumcheckProveFS poly _claimedSum =
  driveProverFS @p (sumcheckProver poly)

-- | Non-interactive sumcheck prover for the product of two MLEs.
sumcheckProveFSProduct ::
  forall p.
  (KnownNat p) =>
  MultilinearPoly (Fp p) ->
  MultilinearPoly (Fp p) ->
  SumcheckProof (Fp p)
sumcheckProveFSProduct a b =
  driveProverFS @p (sumcheckProverProduct a b)

-- | Internal helper: drive any 'ProverState' to completion using Fiat-Shamir.
driveProverFS ::
  forall p.
  (KnownNat p) =>
  ProverState (Fp p) ->
  SumcheckProof (Fp p)
driveProverFS prover =
  evalState (go prover []) emptyTranscript
  where
    go :: ProverState (Fp p) -> [RoundMessage (Fp p)] -> State (Transcript (Fp p)) (SumcheckProof (Fp p))
    go (ProverDone v) acc = return $ SumcheckProof (reverse acc) v
    go (ProverRound msg k) acc = do
      -- Append the round polynomial coefficients directly to the transcript!
      appendToTranscript (polyCoeffs (roundPoly msg))
      -- Derive the challenge algebraically from the transcript
      r <- challenge
      -- Continue with the next round
      go (k r) (msg : acc)

-- | Non-interactive sumcheck verifier using the Fiat-Shamir heuristic.
sumcheckVerifyFS ::
  forall p.
  (KnownNat p) =>
  Int ->
  Fp p ->
  Oracle (Fp p) ->
  SumcheckProof (Fp p) ->
  Verdict
sumcheckVerifyFS numVars claimedSum oracle proof
  | numVars /= length (proofRounds proof) =
      Reject "sumcheckVerifyFS: number of rounds does not match number of variables"
  | otherwise =
      evalState (go claimedSum (proofRounds proof) []) emptyTranscript
  where
    go ::
      Fp p ->
      [RoundMessage (Fp p)] ->
      [Fp p] ->
      State (Transcript (Fp p)) Verdict
    go expectedSum [] challengesAcc =
      if expectedSum == proofFinalEval proof
        then
          let allChallenges = reverse challengesAcc
           in if expectedSum == oracle allChallenges
                then return Accept
                else return $ Reject "Final check: oracle evaluation does not match proof final evaluation"
        else return $ Reject "Final check: expected sum does not match proof final evaluation"
    go expectedSum (RoundMessage g_i : rounds) challengesAcc = do
      let g_sum = polyEval g_i 0 + polyEval g_i 1
      if g_sum /= expectedSum
        then
          return $
            Reject $
              "Round "
                ++ show (numVars - length rounds)
                ++ ": expected sum "
                ++ show expectedSum
                ++ " but got "
                ++ show g_sum
        else do
          -- Append the same polynomial coefficients the prover appended
          appendToTranscript (polyCoeffs g_i)
          -- Re-derive the same challenge
          r <- challenge
          go (polyEval g_i r) rounds (r : challengesAcc)
