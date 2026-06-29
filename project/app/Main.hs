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

  -- Example 8: Inner Product Proof (proving something useful!)
  putStrLn $ "\n=== Inner Product Proof ==="
  let vecA = mlp 2 [1, 2, 3, 4] :: MultilinearPoly (Fp 97)
  let vecB = mlp 2 [5, 6, 7, 8] :: MultilinearPoly (Fp 97)
  -- Actual dot product: 1*5 + 2*6 + 3*7 + 4*8 = 5 + 12 + 21 + 32 = 70
  let dotProduct = 70 :: Fp 97
  putStrLn $ "  Vector a = [1, 2, 3, 4]"
  putStrLn $ "  Vector b = [5, 6, 7, 8]"
  putStrLn $ "  Claimed <a, b> = " ++ show dotProduct

  -- Non-interactive proof using Fiat-Shamir
  let ipProof = sumcheckProveFSProduct @97 vecA vecB
  let ipOracle pt = mleEval vecA pt * mleEval vecB pt
  let ipVerdict = sumcheckVerifyFS @97 2 dotProduct ipOracle ipProof
  putStrLn $ "  Proof = " ++ show ipProof
  putStrLn $ "  Verdict = " ++ show ipVerdict

  -- Show that a wrong claim gets rejected
  let wrongDot = 42 :: Fp 97
  let wrongVerdict = sumcheckVerifyFS @97 2 wrongDot ipOracle ipProof
  putStrLn $ "  Wrong claim <a, b> = " ++ show wrongDot ++ " -> " ++ show wrongVerdict

  -- Example 9: Full ZK scenario — prover and verifier with clear roles
  putStrLn ""
  putStrLn "=== Zero-Knowledge Inner Product: Alice proves to Bob ==="
  putStrLn ""

  -- ── SETUP ──────────────────────────────────────────────────────────
  -- Alice has two private vectors. She wants to prove their dot product
  -- to Bob WITHOUT revealing the vectors themselves.
  let aliceSecretA = mlp 3 [3, 1, 4, 1, 5, 9, 2, 6] :: MultilinearPoly (Fp 97)
  let aliceSecretB = mlp 3 [2, 7, 1, 8, 2, 8, 1, 8] :: MultilinearPoly (Fp 97)
  -- True inner product: 3*2 + 1*7 + 4*1 + 1*8 + 5*2 + 9*8 + 2*1 + 6*8
  --                    =  6  +  7  +  4  +  8  + 10  + 72  +  2  + 48  = 157
  -- 157 mod 97 = 60
  let trueDotProduct = 60 :: Fp 97

  putStrLn "  ┌─────────────────────────────────────────────────┐"
  putStrLn "  │  ALICE (Prover) — knows the secret vectors      │"
  putStrLn "  │    Secret a = [3, 1, 4, 1, 5, 9, 2, 6]          │"
  putStrLn "  │    Secret b = [2, 7, 1, 8, 2, 8, 1, 8]          │"
  putStrLn $ "  │    Claims: <a, b> = " ++ show trueDotProduct ++ "  (mod 97)                │"
  putStrLn "  └─────────────────────────────────────────────────┘"
  putStrLn ""

  -- ── PROVER (Alice) ─────────────────────────────────────────────────
  -- Alice generates a non-interactive proof. She uses ONLY her private
  -- vectors. No interaction with Bob is needed.
  let aliceProof = sumcheckProveFSProduct @97 aliceSecretA aliceSecretB
  putStrLn "  Alice generates proof (Fiat-Shamir, no interaction needed)..."
  putStrLn $ "    Round polynomials: " ++ show (length (proofRounds aliceProof)) ++ " rounds"
  putStrLn $ "    Final evaluation claim: " ++ show (proofFinalEval aliceProof)
  putStrLn ""

  -- ── Alice sends to Bob: the proof + the claimed dot product ────────
  -- She does NOT send her vectors!
  putStrLn "  Alice sends to Bob:"
  putStrLn "    ✓ The proof (round polynomials + final eval)"
  putStrLn $ "    ✓ The claim: <a, b> = " ++ show trueDotProduct
  putStrLn "    ✗ NOT the secret vectors a and b"
  putStrLn ""

  -- ── VERIFIER (Bob) ─────────────────────────────────────────────────
  -- Bob verifies the proof. He needs oracle access to evaluate
  -- a(r)*b(r) at a single random point r — in practice this would be
  -- provided via a polynomial commitment scheme. Here we simulate it.
  putStrLn "  ┌─────────────────────────────────────────────────┐"
  putStrLn "  │  BOB (Verifier) — does NOT know the vectors     │"
  putStrLn "  │    Has: the proof + claimed dot product         │"
  putStrLn "  │    Has: oracle access (1 evaluation of a·b)     │"
  putStrLn "  └─────────────────────────────────────────────────┘"
  putStrLn ""

  let bobOracle pt = mleEval aliceSecretA pt * mleEval aliceSecretB pt
  let bobVerdict = sumcheckVerifyFS @97 3 trueDotProduct bobOracle aliceProof

  putStrLn $ "  Bob's verdict: " ++ show bobVerdict
  putStrLn ""

  -- ── Bob tries a fake claim ─────────────────────────────────────────
  let fakeClaim = 42 :: Fp 97
  let fakeVerdict = sumcheckVerifyFS @97 3 fakeClaim bobOracle aliceProof
  putStrLn $ "  If someone claims <a, b> = " ++ show fakeClaim ++ " with the same proof:"
  putStrLn $ "    Bob's verdict: " ++ show fakeVerdict

