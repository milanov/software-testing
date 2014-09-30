module Sol1Spec(spec) where

import Test.Hspec
import Test.Hspec.HUnit (fromHUnitTest)
import Test.HUnit

import Helper
import Sol1


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- Exercise 1.9
  describe "maxInt" $ do
    fromHUnitTest testSuiteMaxInt

  -- Exercise 1.15
  describe "srtString" $ do
    fromHUnitTest testSuiteSrtString

  -- Exercise 1.17
  describe "substring" $ do
    fromHUnitTest testSuiteSubstring

  -- Exercise 1.20
  describe "lengths" $ do
    fromHUnitTest testSuiteLengths

  -- Exercise 1.21
  describe "sumLengths" $ do
    fromHUnitTest testSuiteSumLengths


testSuiteMaxInt :: Test
testSuiteMaxInt = TestList [
  TestLabel "maxInt" test_maxInt,
  TestLabel "maxIntSingle" test_maxIntSingle,
  TestLabel "maxIntNegative" test_maxIntNegative,
  TestLabel "maxIntEmptyList" test_maxIntEmptyList
  ]

testSuiteSrtString :: Test
testSuiteSrtString = TestList [
  TestLabel "srtString" test_srtString,
  TestLabel "srtStringEmpty" test_srtStringEmpty,
  TestLabel "srtStringEqualFirstLetters" test_srtStringEqualFirstLetters
  ]

testSuiteSubstring :: Test
testSuiteSubstring = TestList [
  TestLabel "substring" test_substring,
  TestLabel "substringSame" test_substringSame,
  TestLabel "substringBeginning" test_substringBeginning,
  TestLabel "substringEnd" test_substringEnd,
  TestLabel "substringFalsy" test_substringFalsy,
  TestLabel "substringEmptyLast" test_substringEmptyLast,
  TestLabel "substringEmptyFirst" test_substringEmptyFirst
  ]

testSuiteLengths :: Test
testSuiteLengths = TestList [
  TestLabel "lengths" test_lengths,
  TestLabel "lengthsSingle" test_lengthsSingle,
  TestLabel "lengthsEmptyLists" test_lengthsEmptyLists
  ]

testSuiteSumLengths :: Test
testSuiteSumLengths = TestList [
  TestLabel "sumLengths" test_sumLengths,
  TestLabel "sumLengthsEmptyList" test_sumLengthsEmptyList,
  TestLabel "sumLengthsSingle" test_sumLengthsSingle
  ]


-- Exercise 1.9
test_maxInt = TestCase $ assertEqual "maxInt"  mx (maxInt [1, 2, 3, mx]) where mx = 14
test_maxIntSingle = TestCase $ assertEqual "maxIntSingle" mx (maxInt [mx]) where mx = 17
test_maxIntNegative = TestCase $ assertEqual "maxIntNegative" mx (maxInt [-2, -3, mx]) where mx = -1
test_maxIntEmptyList = TestCase $ assertError "Can't find maximum of empty list" ( maxInt [] )


-- Exercise 1.15
test_srtString = TestCase $ assertEqual "srtString" ["a", "bc", "d"] (srtString ["d", "a", "bc"])
test_srtStringEmpty = TestCase $ assertEqual "srtStringEmpty" [] (srtString [])
test_srtStringEqualFirstLetters = TestCase $ assertEqual "srtStringEqualFirstLetters" ["a", "aa", "aaa"] (srtString ["aaa", "aa", "a"])


-- Exercise 1.17
test_substring = TestCase $ assertTrue $ substring "qwe" "1qwerty"
test_substringSame = TestCase $ assertTrue $ substring "qwe" "qwe"
test_substringBeginning = TestCase $ assertTrue $ substring "123" "123test"
test_substringEnd = TestCase $ assertTrue $ substring "123" "test123"
test_substringFalsy = TestCase $ assertFalse $ substring "qwe" "123"
test_substringEmptyLast = TestCase $ assertFalse $ substring "qwe" ""
test_substringEmptyFirst = TestCase $ assertTrue $ substring "" "123"


-- Exercise 1.20
test_lengths = TestCase $ assertEqual "lengths" [1, 2, 3] (lengths ["a", "bc", "def"])
test_lengthsSingle = TestCase $ assertEqual "lengthsSingle" [2] (lengths [[True, False]])
test_lengthsEmptyLists = TestCase $ assertEqual "lengthsEmptyLists" [0, 1] (lengths [[], [True]])

-- Exercise 1.21
test_sumLengths = TestCase $ assertEqual "sumLengths" 6 (sumLengths ["a", "bc", "def"])
test_sumLengthsEmptyList = TestCase $ assertEqual "sumLengthsEmptyList" 0 (sumLengths [[]])
test_sumLengthsSingle = TestCase $ assertEqual "sumLengthsSingle" 2 (sumLengths ["12"])