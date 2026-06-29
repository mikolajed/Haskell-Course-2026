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
-- The prover and verifier build identical transcripts, so the verifier can
-- independently re-derive every challenge and check the proof without
-- interaction.
module ZKAlgebra.Crypto.FiatShamir
  ( -- * Transcript
    Transcript (..),
    emptyTranscript,

    -- * Transcript operations
    appendToTranscript,
    challenge,

    -- * Encoding
    encodeFp,
    encodePoly,

    -- * Non-interactive sumcheck
    sumcheckProveFS,
    sumcheckProveFSProduct,
    sumcheckVerifyFS,
  )
where

import Control.Monad.State.Strict (State, get, modify', evalState, runState)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import GHC.TypeNats (KnownNat)
import ZKAlgebra.Crypto.Sumcheck
import ZKAlgebra.Field
import ZKAlgebra.Multilinear
import ZKAlgebra.Polynomial

-------------------------------------------------------------------------------
-- Transcript
-------------------------------------------------------------------------------

-- | An append-only transcript of all protocol messages exchanged so far.
-- Used as the hash input for deriving challenges.
newtype Transcript = Transcript ByteString
  deriving (Eq, Show)

-- | An empty transcript to start a protocol run.
emptyTranscript :: Transcript
emptyTranscript = Transcript BS.empty

-------------------------------------------------------------------------------
-- Transcript operations (State monad)
-------------------------------------------------------------------------------

-- | Append raw bytes to the transcript.
appendToTranscript :: ByteString -> State Transcript ()
appendToTranscript bs = modify' $ \(Transcript t) -> Transcript (t <> bs)

-- | Derive a challenge field element from the current transcript state.
--
-- Hashes the transcript with SHA-256, interprets the first 8 bytes as a
-- big-endian unsigned integer, and reduces modulo @p@. The hash digest is
-- then appended to the transcript as a domain separator so that subsequent
-- calls to 'challenge' produce different values.
challenge :: forall p. (KnownNat p) => State Transcript (Fp p)
challenge = do
  Transcript t <- get
  let digest = SHA256.hash t
      -- Take first 8 bytes and interpret as big-endian Integer
      value = bytesToInteger (BS.take 8 digest)
      result = mkFp value
  -- Append the full digest as a domain separator
  appendToTranscript digest
  return result

-------------------------------------------------------------------------------
-- Encoding
-------------------------------------------------------------------------------

-- | Encode a prime field element as a ByteString.
-- Uses a simple variable-length big-endian encoding of the underlying integer.
encodeFp :: Fp p -> ByteString
encodeFp fp = integerToBytes (unFp fp)

-- | Encode a polynomial as a ByteString by encoding its degree followed by
-- each coefficient.
encodePoly :: Poly (Fp p) -> ByteString
encodePoly p =
  let coeffs = polyCoeffs p
      encodedDeg = integerToBytes (fromIntegral (length coeffs))
      encodedCoeffs = map encodeFp coeffs
   in BS.concat (encodedDeg : encodedCoeffs)

-------------------------------------------------------------------------------
-- Non-interactive sumcheck
-------------------------------------------------------------------------------

-- | Non-interactive sumcheck prover using the Fiat-Shamir heuristic.
--
-- Drives the coroutine-based 'sumcheckProver' by deriving each challenge
-- from the transcript.
sumcheckProveFS ::
  forall p.
  (KnownNat p) =>
  MultilinearPoly (Fp p) ->
  Fp p ->
  SumcheckProof (Fp p)
sumcheckProveFS poly _claimedSum =
  driveProverFS @p (sumcheckProver poly)

-- | Non-interactive sumcheck prover for the product of two MLEs.
--
-- Proves that @\sum_{x \in \{0,1\}^n} a(x) \cdot b(x) = H@ without interaction.
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
    go :: ProverState (Fp p) -> [RoundMessage (Fp p)] -> State Transcript (SumcheckProof (Fp p))
    go (ProverDone v) acc = return $ SumcheckProof (reverse acc) v
    go (ProverRound msg k) acc = do
      -- Append the round polynomial to the transcript
      appendToTranscript (encodePoly (roundPoly msg))
      -- Derive the challenge from the transcript
      r <- challenge @p
      -- Continue with the next round
      go (k r) (msg : acc)

-- | Non-interactive sumcheck verifier using the Fiat-Shamir heuristic.
--
-- Re-derives the challenges from the proof transcript and checks each round,
-- then verifies the final evaluation against the oracle.
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
      State Transcript Verdict
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
          -- Append the same polynomial the prover appended
          appendToTranscript (encodePoly g_i)
          -- Re-derive the same challenge
          r <- challenge @p
          go (polyEval g_i r) rounds (r : challengesAcc)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Convert up to 8 bytes (big-endian) to a non-negative Integer.
bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

-- | Convert a non-negative Integer to a ByteString (big-endian, minimum bytes).
-- Zero is encoded as a single 0x00 byte.
integerToBytes :: Integer -> ByteString
integerToBytes 0 = BS.singleton 0
integerToBytes n = BS.pack (go n [])
  where
    go 0 acc = acc
    go x acc = go (x `div` 256) (fromIntegral (x `mod` 256) : acc)
