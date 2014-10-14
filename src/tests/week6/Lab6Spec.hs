module Lab6Spec(spec) where

import Test.Hspec
import Test.QuickCheck

import Lab6


spec :: Spec
spec = do
  describe "exMFast" $ do
    it "computes its result correctly" $ property $
      (\(NonNegative x) (NonNegative y) (Positive z) -> exMFast x y z == rem (x ^ y) z)