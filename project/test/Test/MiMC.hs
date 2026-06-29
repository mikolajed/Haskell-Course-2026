{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.MiMC (mimcTests) where

import GHC.TypeNats (KnownNat)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ZKAlgebra.Crypto.MiMC
import ZKAlgebra.Field

-- | Arbitrary instance for Field testing
instance KnownNat p => Arbitrary (Fp p) where
  arbitrary = fromInteger <$> arbitrary

mimcTests :: TestTree
mimcTests = testGroup "ZKAlgebra.Crypto.MiMC"
  [ testGroup "Determinism & Soundness"
      [ testProperty "Hashing is deterministic (same input -> same output)" $
          \(x :: Fp 251) -> mimcHash x == mimcHash x
      , testProperty "Hash(x) != Hash(x+1) for almost all x" $
          \(x :: Fp 251) -> mimcHash x /= mimcHash (x + 1)
      ],
    testGroup "Unit Tests (F_97 and F_251)"
      [ testCase "Hash of 0 in F_97" $ do
          let h = mimcHash (0 :: Fp 97)
          -- Just check it evaluates to something and doesn't crash
          h `seq` return ()
      , testCase "Hash of 42 in F_251" $ do
          let h1 = mimcHash (42 :: Fp 251)
          let h2 = mimcHash (43 :: Fp 251)
          assertBool "Avalanche effect" (h1 /= h2)
      ]
  ]
