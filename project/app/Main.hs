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

  putStrLn "\nTry changing Main.hs to experiment with other primes!"
