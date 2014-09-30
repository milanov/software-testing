module Lab4Spec(spec, list2set) where

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

  -- Exercise 4


  -- Exercise 5
  describe "trClos" $ do
    it "returns a transitive relation" $ property $
      prop_trClos


-- Exercise 1
prop_set :: Set Int -> Bool
prop_set (Set s) = s == nub s


-- Exercise 4


-- Exercise 5
prop_trClos :: [(Int, Int)] -> Bool
prop_trClos = transR . trClos