module Lab1BonusSpec(spec) where

import Test.Hspec
import Test.Hspec.HUnit (fromHUnitTest)
import Test.HUnit

import Helper
import Lab1Bonus

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- Exercise 1
  describe "length" $ do
    fromHUnitTest testSuiteLength

  describe "elem" $ do
    fromHUnitTest testSuiteElem

  describe "or" $ do
    fromHUnitTest testSuiteOr

  describe "map" $ do
    fromHUnitTest testSuiteMap

  describe "filter" $ do
    fromHUnitTest testSuiteFilter

  describe "plusplus" $ do
    fromHUnitTest testSuitePlusPlus

  -- Exercise 2 (plus Exercise 1.7)
  describe "reverse" $ do
    fromHUnitTest testSuiteReverse


testSuiteLength :: Test
testSuiteLength = TestList [
  TestLabel "length" test_length,
  TestLabel "lengthEmptyList" test_lengthEmptyList,
  TestLabel "lengthSingleElement" test_lengthSingleElement
  ]

testSuiteElem :: Test
testSuiteElem = TestList [
  TestLabel "elem" test_elem,
  TestLabel "elemFalsy" test_elemFalsy,
  TestLabel "elemSingle" test_elemSingle,
  TestLabel "elemEmptyList" test_elemEmptyList
  ]
  
testSuiteOr :: Test
testSuiteOr = TestList [
  TestLabel "or" test_or,
  TestLabel "orFalsy" test_orFalsy,
  TestLabel "orSingleElement" test_orSingleElement,
  TestLabel "orSingleElementFalsy" test_orSingleElementFalsy,
  TestLabel "orEmptyList" test_orEmptyList
  ]
  
testSuiteMap :: Test
testSuiteMap = TestList [
  TestLabel "map" test_map,
  TestLabel "mapFunc" test_mapFunc,
  TestLabel "mapSingle" test_mapSingle,
  TestLabel "mapEmptyList" test_mapEmptyList
  ]

testSuiteFilter :: Test
testSuiteFilter = TestList [
  TestLabel "filter" test_filter,
  TestLabel "filterAll" test_filterAll,
  TestLabel "filterNone" test_filterNone,
  TestLabel "filterSingle" test_filterSingle
  ]

testSuitePlusPlus :: Test
testSuitePlusPlus = TestList [
  TestLabel "plusplus" test_plusplus,
  TestLabel "plusplusSizes" test_plusplusSizes,
  TestLabel "plusplusEmptyLeft" test_plusplusEmptyLeft,
  TestLabel "plusplusEmptyRight" test_plusplusEmptyRight,
  TestLabel "plusplusBothEmpty" test_plusplusBothEmpty
  ]

testSuiteReverse :: Test
testSuiteReverse = TestList [
  TestLabel "reverse'" test_reverse',
  TestLabel "reverse''" test_reverse'',
  TestLabel "reverseEmpty'" test_reverseEmpty',
  TestLabel "reverseEmpty''" test_reverseEmpty'', 
  TestLabel "reverseSingle'" test_reverseSingle',
  TestLabel "reverseSingle''" test_reverseSingle''
  ]


-- Exercise 1
-- 1.1
test_length = TestCase $ assertEqual "length" 3 (length' "abc")
test_lengthEmptyList = TestCase $ assertEqual "lengthEmptyList" 0 (length' [])
test_lengthSingleElement = TestCase $ assertEqual "lengthSingleElement" 1 (length' [True])

-- 1.2
test_elem = TestCase $ assertTrue $ elem' mx [1, 2, mx] where mx = 5
test_elemFalsy = TestCase $ assertFalse $ elem' 5 [1, 2, 3]
test_elemSingle = TestCase $ assertTrue $ elem' 'a' "a"
test_elemEmptyList = TestCase $ assertFalse $ elem' 1 []

-- 1.3
test_or = TestCase $ assertTrue $ or' [True, False]
test_orFalsy = TestCase $ assertFalse $ or' [False, False, False]
test_orSingleElement = TestCase $ assertTrue $ or' [True]
test_orSingleElementFalsy = TestCase $ assertFalse $ or' [False]
test_orEmptyList = TestCase $ assertFalse $ or' []

-- 1.4
test_map = TestCase $ assertEqual "map" mx (map' id mx) where mx = [1,2,3]
test_mapFunc = TestCase $ assertEqual "mapFunc" [1, 4, 9] (map' (^2) [1, 2, 3])
test_mapSingle = TestCase $ assertEqual "mapSingle" [False] (map' not [True])
test_mapEmptyList = TestCase $ assertEqual "mapEmptyList" [] (map' not [])

-- 1.5
test_filter = TestCase $ assertEqual "filter" [True, True] (filter' (== True) [True, False, True, False])
test_filterAll = TestCase $ assertEqual "filterAll" [] (filter' even [1, 3, 5])
test_filterNone = TestCase $ assertEqual "filterNone" mx (filter' (== 2) mx) where mx = [2, 2, 2]
test_filterSingle = TestCase $ assertEqual "filterSingle" mx (filter' odd mx) where mx = [5]

-- 1.6
test_plusplus = TestCase $ assertEqual "plusplus" [1, 2] (plusplus [1] [2])
test_plusplusSizes = TestCase $ assertEqual "plusplusSizes" "Alan Turing" (plusplus "Alan " "Turing")
test_plusplusEmptyLeft = TestCase $ assertEqual "plusplusEmptyLeft" mx (plusplus [] mx) where mx = [True, False]
test_plusplusEmptyRight = TestCase $ assertEqual "plusplusEmptyRight" mx (plusplus mx "") where mx = "test"
test_plusplusBothEmpty = TestCase $ assertEqual "plusplusBothEmpty" mx (plusplus mx mx) where mx = [] :: [Bool]

-- 1.7 and Exercise 2
test_reverse' = TestCase $ assertEqual "reverse'" [1, 2, 3] (reverse' [3, 2, 1])
test_reverse'' = TestCase $ assertEqual "reverse''" [1, 2, 3] (reverse'' [3, 2, 1])
test_reverseEmpty' = TestCase $ assertEqual "reverseEmpty'" [] (reverse' [] :: [Bool])
test_reverseEmpty'' = TestCase $ assertEqual "reverseEmpty''" [] (reverse'' [] :: [Bool])
test_reverseSingle' = TestCase $ assertEqual "reverseSingle'" mx (reverse' mx) where mx = "a"
test_reverseSingle'' = TestCase $ assertEqual "reverseSingle''" mx (reverse'' mx) where mx = "a"