module Main (main) where

import Test.Tasty
import Test.Field (fieldTests)

main :: IO ()
main = defaultMain $ testGroup "ZKAlgebra"
  [ fieldTests
  ]
