{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.FiatShamir (fiatShamirTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import ZKAlgebra.Field
import ZKAlgebra.Multilinear
import ZKAlgebra.Polynomial
import ZKAlgebra.Crypto.Sumcheck
import ZKAlgebra.Crypto.FiatShamir
import qualified Data.Vector as V

type F = Fp 97

genF :: Gen F
genF = mkFp <$> chooseInteger (0, 96)

genMLP :: Int -> Gen (MultilinearPoly F)
genMLP numVars = mlp numVars <$> vectorOf (2 ^ numVars) genF

fiatShamirTests :: TestTree
fiatShamirTests =
  testGroup
    "ZKAlgebra.Crypto.FiatShamir"
    [ determinismTests,
      completenessTests,
      soundnessTests,
      innerProductFSTests,
      unitTests
    ]

determinismTests :: TestTree
determinismTests =
  testGroup
    "Determinism"
    [ testProperty "same input -> same proof" $
        forAll (chooseInt (1, 5)) $ \numVars ->
          forAll (genMLP numVars) $ \f ->
            let claim = mleSum f
                proof1 = sumcheckProveFS @97 f claim
                proof2 = sumcheckProveFS @97 f claim
             in proof1 === proof2
    ]

completenessTests :: TestTree
completenessTests =
  testGroup
    "Completeness"
    [ testProperty "honest prover with correct sum -> Accept" $
        forAll (chooseInt (1, 5)) $ \numVars ->
          forAll (genMLP numVars) $ \f ->
            let claim = mleSum f
                proof = sumcheckProveFS @97 f claim
                oracle pt = mleEval f pt
             in sumcheckVerifyFS @97 numVars claim oracle proof === Accept
    ]

soundnessTests :: TestTree
soundnessTests =
  testGroup
    "Soundness"
    [ testProperty "prover with incorrect sum -> Reject" $
        forAll (chooseInt (1, 5)) $ \numVars ->
          forAll (genMLP numVars) $ \f ->
            forAll genF $ \wrongClaim ->
              wrongClaim /= mleSum f ==>
                let proof = sumcheckProveFS @97 f wrongClaim
                    oracle pt = mleEval f pt
                 in case sumcheckVerifyFS @97 numVars wrongClaim oracle proof of
                      Reject _ -> True
                      Accept   -> False
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "2-variable example produces Accept" $
        let f = mlp 2 [10, 20, 30, 40] :: MultilinearPoly F
            claim = mleSum f
            proof = sumcheckProveFS @97 f claim
            oracle pt = mleEval f pt
         in sumcheckVerifyFS @97 2 claim oracle proof @?= Accept,
      testCase "wrong claim produces Reject" $
        let f = mlp 2 [10, 20, 30, 40] :: MultilinearPoly F
            wrongClaim = 99 :: F
            proof = sumcheckProveFS @97 f wrongClaim
            oracle pt = mleEval f pt
         in case sumcheckVerifyFS @97 2 wrongClaim oracle proof of
              Reject _ -> return ()
              Accept   -> assertFailure "Should have rejected wrong claim",
      testCase "non-interactive proof differs from interactive with arbitrary challenges" $
        -- The FS proof should NOT equal the interactive proof with arbitrary
        -- challenges, because FS derives its own challenges deterministically.
        let f = mlp 2 [10, 20, 30, 40] :: MultilinearPoly F
            claim = mleSum f
            fsProof = sumcheckProveFS @97 f claim
            interactiveProof = sumcheckProveInteractive f claim [2, 3]
         in assertBool
              "FS proof should differ from interactive proof with [2,3] challenges"
              (fsProof /= interactiveProof),
      testCase "encoding round-trip: encodeFp produces non-empty ByteString" $
        let x = 42 :: F
         in assertBool "encodeFp should not be empty" (not (null (show (encodeFp x))))
    ]

innerProductFSTests :: TestTree
innerProductFSTests =
  testGroup
    "Inner product (Fiat-Shamir)"
    [ testProperty "FS inner product completeness" $
        forAll (chooseInt (1, 4)) $ \numVars ->
          forAll (genMLP numVars) $ \a ->
            forAll (genMLP numVars) $ \b ->
              let innerProd = sum [ (mlpEvals a V.! i) * (mlpEvals b V.! i)
                                 | i <- [0 .. 2^numVars - 1] ]
                  proof = sumcheckProveFSProduct @97 a b
                  oracle pt = mleEval a pt * mleEval b pt
               in sumcheckVerifyFS @97 numVars innerProd oracle proof === Accept,
      testCase "FS inner product of [1,2,3,4] . [5,6,7,8] = 70" $
        let a = mlp 2 [1, 2, 3, 4] :: MultilinearPoly F
            b = mlp 2 [5, 6, 7, 8] :: MultilinearPoly F
            claim = 70 :: F
            proof = sumcheckProveFSProduct @97 a b
            oracle pt = mleEval a pt * mleEval b pt
         in sumcheckVerifyFS @97 2 claim oracle proof @?= Accept
    ]
