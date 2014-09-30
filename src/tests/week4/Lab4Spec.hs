{-# LANGUAGE TypeOperators #-}

module Lab4Spec(spec, list2set) where

import Test.Hspec
import Test.QuickCheck

import Lab4
import SetOrd
import Data.List


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- Exercise 3
  describe "getRandomSet" $ do
    it "generates sets that contain no duplicate elements" $ property $
      prop_set

  -- Exercise 4
  describe "unionSet" $ do
    it "contains all the elements from the two sets" $ property $
      prop_setUnionContainsAllElements

    it "is a set" $ property $ do
      prop_set

    it "returns the original set when you union it with itself" $ property $
      prop_unionSetSameSet

    it "returns the original set when you union it with the empty set" $ property $
      prop_unionSetEmptySet

    it "satisfies the commutative property" $ property $
      prop_unionSetIsCommutative

  describe "intersectionSet" $ do
    it "contains only elements from the two sets" $ property $
      prop_intersectionSetContainsElementsFromSets

    it "is a set" $ property $ do
      prop_set

    it "returns the original set when you intersect it with itself" $ property $
      prop_intersectionSetSameSet

    it "returns the empty set when you intersect it with the empty set" $ property $
      prop_intersectionSetEmptySet

    it "satisfies the commutative property" $ property $
      prop_intersectionSetIsCommutative

  describe "differenceSet" $ do
    it "contains elements which are in the first set" $ property $
      prop_differenceSetContainsElementsFromFirstSet

    it "doesn't contain elements from the second set" $ property $
      prop_differenceSetDoesntContainElementsFromSecondSet

    it "is a set" $ property $ do
      prop_set

    it "returns the empty set when you differ it with itself" $ property $
      prop_differenceSetSameSet

    it "returns the same set when you differ it with the empty set" $ property $
      prop_differenceSetEmptySet

  describe "trClos" $ do
    it "returns an empty transitive closure for an empty relation" $ do
      let mx = [] :: [(Int, Int)]
      trClos mx `shouldBe` mx

    it "returns the same relation when it contains only one element" $ do
      trClos [(1,2)] `shouldBe` [(1,2)]

    it "returns returns the same relation when there is no transitive relation between any of the elements" $ do
      trClos [(1,2), (3,4)] `shouldBe` [(1,2), (3,4)]

    it "returns the correct transitive closure for a relation" $ do
      (sort $ trClos [(1,2), (2,3), (4,5)]) `shouldBe` [(1,2), (1,3), (2,3), (4,5)]

    -- Exercise 7
    it "returns a transitive closure with unique elements" $ property $
      prop_trClosUnique

    it "returns a transitive closure with bigger (or equal) size than the original relation" $ property $
      prop_trClosLengths

    it "returns a transitive closure which contains the elements of the original relation" $ property $
      prop_trClosSublist

    it "returns a transitive relation" $ property $
      prop_trClos


-- Exercise 1
prop_set :: Set Int -> Bool
prop_set (Set s) = s == nub s


-- Exercise 4
prop_setUnionContainsAllElements :: Set Int -> Set Int -> Bool
prop_setUnionContainsAllElements (Set a) (Set b) = all (\ x -> inSet x (unionSet (Set a) (Set b))) (a ++ b)

prop_unionSetSameSet :: Set Int -> Bool
prop_unionSetSameSet a = unionSet a a == a

prop_unionSetEmptySet :: Set Int -> Bool
prop_unionSetEmptySet a = unionSet a (Set []) == a

prop_unionSetIsCommutative :: Set Int -> Set Int -> Bool
prop_unionSetIsCommutative a b = unionSet a b == unionSet b a

prop_intersectionSetContainsElementsFromSets :: Set Int -> Set Int -> Bool
prop_intersectionSetContainsElementsFromSets (Set a) (Set b) = let Set intersection = intersectionSet (Set a) (Set b) in
                                                               all (\ x -> inSet x (Set (a ++ b))) intersection

prop_intersectionSetSameSet :: Set Int -> Bool
prop_intersectionSetSameSet a = intersectionSet a a == a

prop_intersectionSetEmptySet :: Set Int -> Bool
prop_intersectionSetEmptySet a = intersectionSet a (Set []) == Set []

prop_intersectionSetIsCommutative :: Set Int -> Set Int -> Bool
prop_intersectionSetIsCommutative a b = intersectionSet a b == intersectionSet b a

prop_differenceSetContainsElementsFromFirstSet :: Set Int -> Set Int -> Bool
prop_differenceSetContainsElementsFromFirstSet a b = let Set difference = differenceSet a b in
                                                               all (\ x -> inSet x a) difference

prop_differenceSetDoesntContainElementsFromSecondSet :: Set Int -> Set Int -> Bool
prop_differenceSetDoesntContainElementsFromSecondSet a b = let Set difference = differenceSet a b in
                                                               not $ any (\ x -> inSet x b) difference

prop_differenceSetSameSet :: Set Int -> Bool
prop_differenceSetSameSet a = differenceSet a a == Set []

prop_differenceSetEmptySet :: Set Int -> Bool
prop_differenceSetEmptySet a = differenceSet a (Set []) == a


-- Exercise 5
prop_trClos :: [(Int, Int)] -> Bool
prop_trClos = transR . trClos

prop_trClosUnique :: [(Int, Int)] -> Bool
prop_trClosUnique rel = trCl == nub trCl where trCl = trClos rel

prop_trClosSublist :: [(Int, Int)] -> Bool
prop_trClosSublist rel = all ((flip elem) trCl) rel where trCl = trClos rel

prop_trClosLengths :: [(Int, Int)] -> Bool
prop_trClosLengths rel = length (trClos rel) >= length (nub rel)