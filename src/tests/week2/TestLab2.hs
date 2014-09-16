{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestLab2 where

import Test.HUnit.Lang
import Test.Framework
import Helper

import Lab2


-- Exercise 1
test_triangleInvalid = assertEqual NoTriangle (triangle 1 8 1)
test_triangleInvalid2 = assertEqual NoTriangle (triangle 2 3 0)
test_triangleInvalid3 = assertEqual NoTriangle (triangle 12 1 11)
test_triangleInvalidNegative = assertEqual NoTriangle (triangle 3 2 (-3))
test_triangleInvalidNegative2 = assertEqual NoTriangle (triangle (-1) (-2) (-4))
test_triangleEquilateral = assertEqual Equilateral (triangle 2 2 2)
test_triangleIsosceles = assertEqual Isosceles (triangle 2 2 3)
test_triangleIsosceles2 = assertEqual Isosceles (triangle 6 3 6)
test_triangleIsosceles3 = assertEqual Isosceles (triangle 4 9 9)
test_triangleRectangular = assertEqual Rectangular (triangle 3 4 5)
test_triangleRectangular2 = assertEqual Rectangular (triangle 160 231 281)
test_triangleOther = assertEqual Other (triangle 2 3 4)
test_triangleOther2 = assertEqual Other (triangle 7 6 11)


-- Exercise 2
test_isPermutation = assertTrue $ isPermutation [1, 2, 3] [3, 2, 1]
test_isPermutationSame = assertTrue $ isPermutation mx mx where mx = [True, False]
test_isPermutationDuplicates = assertTrue $ isPermutation [1, 2, 3, 3] [3, 3, 1, 2]
test_isPermutationFalsy = assertFalse $ isPermutation ['a', 'b', 'c'] ['a', 'a', 'g']
test_isPermutationDifferentLengths = assertFalse $ isPermutation ['a', 'b'] ['a', 'b', 'c']
test_isPermutationDifferentLengths2 = assertFalse $ isPermutation ['a', 'b', 'c'] ['a', 'b']
test_isPermutationEmptyList = assertFalse $ isPermutation [4, 3, 2] []
test_isPermutationEmptyList2 = assertFalse $ isPermutation [] [4, 3, 2]
test_isPermutationBothEmpty = assertTrue $ isPermutation mx mx where mx = [] :: [Bool]


-- Exercise 8
