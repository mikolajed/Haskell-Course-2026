-- |
-- Module      : ZKAlgebra.Crypto.Sumcheck
-- Description : Interactive Sumcheck Protocol Prover and Verifier
--
-- This module implements the interactive version of the sumcheck protocol.
module ZKAlgebra.Crypto.Sumcheck
  ( -- * Sumcheck types
    RoundMessage (..),
    SumcheckProof (..),
    Verdict (..),
    Oracle,

    -- * Interactive prover (coroutine)
    ProverState (..),
    sumcheckProver,
    sumcheckProverProduct,
    runProver,

    -- * Batch prover (all challenges upfront)
    sumcheckProveInteractive,

    -- * Verifier
    sumcheckVerify,
  )
where

import ZKAlgebra.Algebra
import ZKAlgebra.Multilinear
import ZKAlgebra.Polynomial

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A round message sent from the prover to the verifier.
-- Contains the univariate polynomial g_i(X) for the current round.
data RoundMessage f = RoundMessage
  { roundPoly :: Poly f
  }
  deriving (Eq, Show)

-- | The complete proof for the sumcheck protocol.
-- Contains the messages for all rounds and the prover's final evaluation claim.
data SumcheckProof f = SumcheckProof
  { proofRounds :: [RoundMessage f],
    proofFinalEval :: f
  }
  deriving (Eq, Show)

-- | Verification result.
data Verdict = Accept | Reject String
  deriving (Eq, Show)

-- | An oracle providing the verifier access to evaluations of the original polynomial.
type Oracle f = [f] -> f

-------------------------------------------------------------------------------
-- Interactive prover (coroutine)
-------------------------------------------------------------------------------

-- | The prover's state in a truly interactive protocol.
--
-- This is a coroutine: the prover either yields a round message together with
-- a continuation that awaits the verifier's challenge, or it is done and
-- carries the final evaluation.
--
-- Because the continuation is a function @(f -> ProverState f)@, it is
-- /structurally impossible/ for the prover to inspect future challenges
-- when computing the current round polynomial.
data ProverState f
  = -- | The prover has finished all rounds and provides the final evaluation.
    ProverDone f
  | -- | The prover sends a 'RoundMessage' and suspends, awaiting the next
    -- verifier challenge before continuing.
    ProverRound (RoundMessage f) (f -> ProverState f)

-- | Create an interactive prover from a multilinear polynomial.
--
-- Each step computes the partial-sum polynomial @g_i@ from the /current/
-- polynomial state, then waits for a challenge @r@ before fixing the first
-- variable and continuing.
sumcheckProver :: (Field f) => MultilinearPoly f -> ProverState f
sumcheckProver p
  | mlpNumVars p == 0 = ProverDone (mleEval p [])
  | otherwise =
      let g_i = mlePartialSum p
       in ProverRound (RoundMessage g_i) $ \r ->
            sumcheckProver (mleFix p r)

-- | Create an interactive prover for the product of two multilinear polynomials.
--
-- Used for inner-product proofs: given @a@ and @b@, proves that
-- @\sum_{x \in \{0,1\}^n} a(x) \cdot b(x) = H@.
--
-- Each round computes a degree-2 partial-sum polynomial from the product
-- of @a@ and @b@, then waits for a challenge before fixing the first
-- variable in both polynomials.
sumcheckProverProduct ::
  (Field f) => MultilinearPoly f -> MultilinearPoly f -> ProverState f
sumcheckProverProduct a b
  | mlpNumVars a == 0 = ProverDone (mleEval a [] * mleEval b [])
  | otherwise =
      let g_i = mlePartialSumProduct a b
       in ProverRound (RoundMessage g_i) $ \r ->
            sumcheckProverProduct (mleFix a r) (mleFix b r)

-- | Drive an interactive prover to completion with a list of challenges,
-- collecting the transcript into a 'SumcheckProof'.
runProver :: ProverState f -> [f] -> SumcheckProof f
runProver (ProverDone v) [] = SumcheckProof [] v
runProver (ProverRound msg k) (r : rs) =
  let SumcheckProof rest finalV = runProver (k r) rs
   in SumcheckProof (msg : rest) finalV
runProver (ProverDone _) (_ : _) =
  error "runProver: too many challenges"
runProver (ProverRound _ _) [] =
  error "runProver: not enough challenges"

-------------------------------------------------------------------------------
-- Batch prover (all challenges upfront)
-------------------------------------------------------------------------------

-- | Batch sumcheck prover.
--
-- This is a convenience wrapper around 'sumcheckProver' and 'runProver'
-- that accepts all challenges upfront.
sumcheckProveInteractive ::
  (Field f) =>
  MultilinearPoly f -> -- The original polynomial f
  f ->                 -- Claimed sum H
  [f] ->               -- Verifier's challenges, one per round
  SumcheckProof f
sumcheckProveInteractive poly _claimedSum challenges =
  runProver (sumcheckProver poly) challenges

-- | The interactive sumcheck verifier.
-- Checks the proof round by round, ensuring polynomials sum correctly,
-- and then verifies the final evaluation against the oracle.
sumcheckVerify ::
  (Field f) =>
  Int ->               -- Number of variables
  f ->                 -- Claimed sum H
  Oracle f ->          -- Oracle for evaluating f
  [f] ->               -- Verifier's challenges used
  SumcheckProof f ->   -- The proof
  Verdict
sumcheckVerify numVars claimedSum oracle challenges proof
  | numVars /= length challenges =
      Reject "sumcheckVerify: number of challenges does not match number of variables"
  | numVars /= length (proofRounds proof) =
      Reject "sumcheckVerify: number of rounds in proof does not match number of variables"
  | otherwise =
      go claimedSum challenges (proofRounds proof)
  where
    go expectedSum [] [] =
      if expectedSum == proofFinalEval proof
        then
          if expectedSum == oracle challenges
            then Accept
            else Reject "Final check: oracle evaluation does not match proof final evaluation"
        else Reject "Final check: expected sum does not match proof final evaluation"
    go expectedSum (r : rs) (RoundMessage g_i : rounds) =
      let g_sum = polyEval g_i 0 + polyEval g_i 1
       in if g_sum /= expectedSum
            then Reject $ "Round " ++ show (numVars - length rs) ++ ": expected sum " ++ show expectedSum ++ " but got " ++ show g_sum
            else go (polyEval g_i r) rs rounds
    go _ _ _ = Reject "sumcheckVerify: mismatched lengths"
