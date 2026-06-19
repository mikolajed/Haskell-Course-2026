module Main (main) where

import Test.Tasty
import Test.Field (fieldTests)
import Test.Multilinear (multilinearTests)
import Test.NTT (nttTests)
import Test.Polynomial (polynomialTests)

main :: IO ()
main = defaultMain $ testGroup "ZKAlgebra"
  [ fieldTests
  , multilinearTests
  , nttTests
  , polynomialTests
  ]
