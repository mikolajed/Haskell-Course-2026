{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import ZKAlgebra

main :: IO ()
main = do
  putStrLn "=== ZKAlgebra: Finite Field Arithmetic Demo ==="

  -- Example 1: F_97
  let a = 42 :: Fp 97
  let b = 80 :: Fp 97

  putStrLn $ "\nField: F_97"
  putStrLn $ "a = " ++ show a
  putStrLn $ "b = " ++ show b
  putStrLn $ "a + b = " ++ show (a + b)
  putStrLn $ "a * b = " ++ show (a * b)
  putStrLn $ "a / b = " ++ show (a / b)
  putStrLn $ "a^96  = " ++ show (a ^ (96 :: Int)) ++ " (Fermat's Theorem)"

  -- Example 2: F_251
  let x = 200 :: Fp 251
  let y = 100 :: Fp 251

  putStrLn $ "\nField: F_251"
  putStrLn $ "x + y = " ++ show (x + y)
  putStrLn $ "x * y = " ++ show (x * y)

  -- Example 3: polynomials over F_97
  let p = poly [3, 1, 0, 1] :: Poly (Fp 97)
  let q = poly [1, 1] :: Poly (Fp 97)

  putStrLn $ "\nPolynomial over F_97:"
  putStrLn $ "p(x) = " ++ show p
  putStrLn $ "q(x) = " ++ show q
  putStrLn $ "p(5) = " ++ show (polyEval p 5)
  putStrLn $ "p(x) * q(x) = " ++ show (p * q)
  putStrLn $ "polyMulNTT p q = " ++ show (polyMulNTT p q)

  -- Example 4: multilinear polynomial over F_97
  let f = mlp 2 [10, 20, 30, 40] :: MultilinearPoly (Fp 97)

  putStrLn $ "\nMultilinear polynomial over F_97:"
  putStrLn $ "f evaluations = " ++ show (mlpEvals f)
  putStrLn $ "sum over {0,1}^2 = " ++ show (mleSum f)
  putStrLn $ "f~(2,3) = " ++ show (mleEval f [2, 3])
  putStrLn $ "partial sum over x2 = " ++ show (mlePartialSum f)

  -- Example 5: Interactive Sumcheck Protocol
  let claim = 100 -- 10 + 20 + 30 + 40
  let challenges = [2, 3] :: [Fp 97]
  let proof = sumcheckProveInteractive f claim challenges

  putStrLn $ "\nSumcheck Protocol over {0,1}^2:"
  putStrLn $ "Claimed sum H = " ++ show claim
  putStrLn $ "Verifier challenges = " ++ show challenges
  putStrLn $ "Proof = " ++ show proof
  putStrLn $ "Verdict = " ++ show (sumcheckVerify 2 claim (\pt -> mleEval f pt) challenges proof)

  -- Example 6: Coroutine (truly interactive) Sumcheck Prover
  putStrLn $ "\nCoroutine Sumcheck Prover (step-by-step):"
  case sumcheckProver f of
    ProverDone v -> putStrLn $ "  Immediately done: " ++ show v
    ProverRound (RoundMessage g1) k1 -> do
      putStrLn $ "  Round 1: prover sends g1(X) = " ++ show g1
      putStrLn $ "           verifier checks g1(0)+g1(1) = " ++ show (polyEval g1 0 + polyEval g1 1)
      let r1 = 2 :: Fp 97
      putStrLn $ "           verifier sends challenge r1 = " ++ show r1
      case k1 r1 of
        ProverDone v -> putStrLn $ "  Done: final eval = " ++ show v
        ProverRound (RoundMessage g2) k2 -> do
          putStrLn $ "  Round 2: prover sends g2(X) = " ++ show g2
          putStrLn $ "           verifier checks g2(0)+g2(1) = " ++ show (polyEval g2 0 + polyEval g2 1) ++ " == g1(r1) = " ++ show (polyEval g1 r1)
          let r2 = 3 :: Fp 97
          putStrLn $ "           verifier sends challenge r2 = " ++ show r2
          case k2 r2 of
            ProverDone v ->
              putStrLn $ "  Done: final eval = " ++ show v ++ " (should equal f~(2,3) = " ++ show (mleEval f [2, 3]) ++ ")"
            ProverRound _ _ ->
              putStrLn $ "  Unexpected: more rounds?"

  -- Example 7: Fiat-Shamir (non-interactive) Sumcheck
  putStrLn $ "\nFiat-Shamir Non-Interactive Sumcheck:"
  let fsProof = sumcheckProveFS @97 f claim
  putStrLn $ "  Proof (no challenges needed!) = " ++ show fsProof
  let fsVerdict = sumcheckVerifyFS @97 2 claim (\pt -> mleEval f pt) fsProof
  putStrLn $ "  Verdict = " ++ show fsVerdict

  putStrLn "\nTry changing Main.hs to experiment with other primes!"
