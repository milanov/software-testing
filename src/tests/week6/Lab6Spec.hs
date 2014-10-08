module Lab6Spec(spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List

import Lab6
import Week6


spec :: Spec
spec = do
  it "is pendint" $ do
    pending