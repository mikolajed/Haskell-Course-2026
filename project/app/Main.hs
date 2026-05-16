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

  putStrLn "\nTry changing Main.hs to experiment with other primes!"
