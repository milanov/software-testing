module Lab4Spec(spec) where

import Test.Hspec
import Test.Hspec.HUnit (fromHUnitTest)
import Test.HUnit
import Test.QuickCheck

import Helper
import Lab4
import SetOrd
import Data.List


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- Exercise 3
  describe "Simple set operations" $ do
    it "generates sets that contain no duplicate elements" $ property $
      prop_set

prop_set :: Set Int -> Bool
prop_set (Set s) = s == (nub s)