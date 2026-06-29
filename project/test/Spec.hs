module Main (main) where

import Test.Tasty
import Test.Field (fieldTests)
import Test.Multilinear (multilinearTests)
import Test.NTT (nttTests)
import Test.Polynomial (polynomialTests)
import Test.Sumcheck (sumcheckTests)
import Test.FiatShamir (fiatShamirTests)
import Test.MiMC (mimcTests)

main :: IO ()
main = defaultMain $ testGroup "ZKAlgebra"
  [ fieldTests
  , multilinearTests
  , nttTests
  , polynomialTests
  , sumcheckTests
  , fiatShamirTests
  , mimcTests
  ]
