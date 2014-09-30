module Lab2Spec(spec) where

import Test.Hspec
import Test.Hspec.HUnit (fromHUnitTest)
import Test.HUnit

import Helper
import Lab2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- Exercise 1
  describe "triangle" $ do
    fromHUnitTest testSuiteTriangle

  -- Exercise 2
  describe "isPermutation" $ do
    fromHUnitTest testSuiteIsPermutation


testSuiteTriangle :: Test
testSuiteTriangle = TestList [
  TestLabel "triangleInvalid" test_triangleInvalid,
  TestLabel "triangleInvalid2" test_triangleInvalid2,
  TestLabel "triangleInvalid3" test_triangleInvalid3,
  TestLabel "triangleInvalidNegative" test_triangleInvalidNegative,
  TestLabel "triangleInvalidNegative2" test_triangleInvalidNegative2,
  TestLabel "triangleEquilateral" test_triangleEquilateral,
  TestLabel "triangleIsosceles" test_triangleIsosceles,
  TestLabel "triangleIsosceles2" test_triangleIsosceles2,
  TestLabel "triangleIsosceles3" test_triangleIsosceles3,
  TestLabel "triangleRectangular" test_triangleRectangular,
  TestLabel "triangleRectangular2" test_triangleRectangular2,
  TestLabel "triangleOther" test_triangleOther,
  TestLabel "triangleOther2" test_triangleOther2
  ]

testSuiteIsPermutation :: Test
testSuiteIsPermutation = TestList [
  TestLabel "isPermutation" test_isPermutation,
  TestLabel "isPermutationSame" test_isPermutationSame,
  TestLabel "isPermutationDuplicates" test_isPermutationDuplicates,
  TestLabel "isPermutationDuplicates" test_isPermutationFalsy,
  TestLabel "isPermutationDifferentLengths" test_isPermutationDifferentLengths,
  TestLabel "isPermutationDifferentLengths2" test_isPermutationDifferentLengths2,
  TestLabel "isPermutationEmptyList" test_isPermutationEmptyList,
  TestLabel "isPermutationEmptyList2" test_isPermutationEmptyList2,
  TestLabel "isPermutationBothEmpty" test_isPermutationBothEmpty
  ]

-- Exercise 1
test_triangleInvalid = TestCase $ assertEqual "triangle" NoTriangle (triangle 1 8 1)
test_triangleInvalid2 = TestCase $ assertEqual "triangle" NoTriangle (triangle 2 3 0)
test_triangleInvalid3 = TestCase $ assertEqual "triangle" NoTriangle (triangle 12 1 11)
test_triangleInvalidNegative = TestCase $ assertEqual "triangle" NoTriangle (triangle 3 2 (-3))
test_triangleInvalidNegative2 = TestCase $ assertEqual "triangle" NoTriangle (triangle (-1) (-2) (-4))
test_triangleEquilateral = TestCase $ assertEqual "triangle" Equilateral (triangle 2 2 2)
test_triangleIsosceles = TestCase $ assertEqual "triangle" Isosceles (triangle 2 2 3)
test_triangleIsosceles2 = TestCase $ assertEqual "triangle" Isosceles (triangle 6 3 6)
test_triangleIsosceles3 = TestCase $ assertEqual "triangle" Isosceles (triangle 4 9 9)
test_triangleRectangular = TestCase $ assertEqual "triangle" Rectangular (triangle 3 4 5)
test_triangleRectangular2 = TestCase $ assertEqual "triangle" Rectangular (triangle 160 231 281)
test_triangleOther = TestCase $ assertEqual "triangle" Other (triangle 2 3 4)
test_triangleOther2 = TestCase $ assertEqual "triangle" Other (triangle 7 6 11)


-- Exercise 2
test_isPermutation = TestCase $ assertTrue $ isPermutation [1, 2, 3] [3, 2, 1]
test_isPermutationSame = TestCase $ assertTrue $ isPermutation mx mx where mx = [True, False]
test_isPermutationDuplicates = TestCase $ assertTrue $ isPermutation [1, 2, 3, 3] [3, 3, 1, 2]
test_isPermutationFalsy = TestCase $ assertFalse $ isPermutation ['a', 'b', 'c'] ['a', 'a', 'g']
test_isPermutationDifferentLengths = TestCase $ assertFalse $ isPermutation ['a', 'b'] ['a', 'b', 'c']
test_isPermutationDifferentLengths2 = TestCase $ assertFalse $ isPermutation ['a', 'b', 'c'] ['a', 'b']
test_isPermutationEmptyList = TestCase $ assertFalse $ isPermutation [4, 3, 2] []
test_isPermutationEmptyList2 = TestCase $ assertFalse $ isPermutation [] [4, 3, 2]
test_isPermutationBothEmpty = TestCase $ assertTrue $ isPermutation mx mx where mx = [] :: [Bool]
