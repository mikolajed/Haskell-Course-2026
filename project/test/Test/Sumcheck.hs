{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Sumcheck (sumcheckTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import ZKAlgebra.Field
import ZKAlgebra.Multilinear
import ZKAlgebra.Polynomial
import ZKAlgebra.Crypto.Sumcheck

type F = Fp 97

genF :: Gen F
genF = mkFp <$> chooseInteger (0, 96)

genChallenges :: Int -> Gen [F]
genChallenges n = vectorOf n genF

genMLP :: Int -> Gen (MultilinearPoly F)
genMLP numVars = mlp numVars <$> vectorOf (2 ^ numVars) genF

sumcheckTests :: TestTree
sumcheckTests =
  testGroup
    "ZKAlgebra.Crypto.Sumcheck"
    [ completenessTests,
      soundnessTests,
      coroutineTests,
      unitTests
    ]

completenessTests :: TestTree
completenessTests =
  testGroup
    "Completeness"
    [ testProperty "honest prover with correct sum -> Accept" $
        forAll (chooseInt (1, 5)) $ \numVars ->
          forAll (genMLP numVars) $ \f ->
            forAll (genChallenges numVars) $ \challenges ->
              let claim = mleSum f
                  proof = sumcheckProveInteractive f claim challenges
                  oracle pt = mleEval f pt
               in sumcheckVerify numVars claim oracle challenges proof === Accept
    ]

soundnessTests :: TestTree
soundnessTests =
  testGroup
    "Soundness"
    [ testProperty "prover with incorrect sum -> Reject" $
        forAll (chooseInt (1, 5)) $ \numVars ->
          forAll (genMLP numVars) $ \f ->
            forAll (genChallenges numVars) $ \challenges ->
              forAll genF $ \wrongClaim ->
                wrongClaim /= mleSum f ==>
                  let proof = sumcheckProveInteractive f wrongClaim challenges
                      oracle pt = mleEval f pt
                      -- Verify should reject since the starting sum claim is wrong
                   in case sumcheckVerify numVars wrongClaim oracle challenges proof of
                        Reject _ -> True
                        Accept   -> False
    ]

coroutineTests :: TestTree
coroutineTests =
  testGroup
    "Coroutine prover"
    [ testProperty "coroutine prover matches batch prover" $
        forAll (chooseInt (1, 5)) $ \numVars ->
          forAll (genMLP numVars) $ \f ->
            forAll (genChallenges numVars) $ \challenges ->
              let batchProof = sumcheckProveInteractive f (mleSum f) challenges
                  coroutineProof = runProver (sumcheckProver f) challenges
               in batchProof === coroutineProof,
      testCase "manual stepping through 2-variable prover" $
        let f = mlp 2 [10, 20, 30, 40] :: MultilinearPoly F
            claim = mleSum f
         in case sumcheckProver f of
              ProverDone _ -> assertFailure "Expected ProverRound, got ProverDone"
              ProverRound (RoundMessage g1) k1 -> do
                -- Round 1: g1 should satisfy g1(0) + g1(1) == claim
                polyEval g1 0 + polyEval g1 1 @?= claim
                -- Feed challenge r1 = 2
                case k1 2 of
                  ProverDone _ -> assertFailure "Expected second ProverRound"
                  ProverRound (RoundMessage g2) k2 -> do
                    -- Round 2: g2 should satisfy g2(0) + g2(1) == g1(r1)
                    polyEval g2 0 + polyEval g2 1 @?= polyEval g1 2
                    -- Feed challenge r2 = 3, should finish
                    case k2 3 of
                      ProverDone finalEval ->
                        finalEval @?= mleEval f [2, 3]
                      ProverRound _ _ ->
                        assertFailure "Expected ProverDone after all variables fixed"
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "2-variable example from specification" $
        let f = mlp 2 [10, 20, 30, 40] :: MultilinearPoly F
            claim = 100 -- 10 + 20 + 30 + 40
            challenges = [2, 3]
            proof = sumcheckProveInteractive f claim challenges
            oracle pt = mleEval f pt
         in do
              sumcheckVerify 2 claim oracle challenges proof @?= Accept
              -- test wrong claim
              let badVerdict = sumcheckVerify 2 99 oracle challenges proof
              case badVerdict of
                Reject err -> return () -- expected
                Accept     -> assertFailure "Should have rejected wrong claim"
    ]
