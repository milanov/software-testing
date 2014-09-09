{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestLab1BonusSolutions where

import Test.HUnit.Lang
import Test.Framework
import Helper

import Lab1Bonus

-- Exercise 1. The tests here and perfect for QuickCheck (comparing with the
-- built-in functions from Prelude) and should someday be rewritten using it.

-- 1.1
test_length = assertEqual 3 (length' "abc")
test_lengthEmptyList = assertEqual 0 (length' [])
test_lengthSingleElement = assertEqual 1 (length' [True])

-- 1.2
test_elem = assertTrue $ elem' mx [1, 2, mx] where mx = 5
test_elemFalsy = assertFalse $ elem' 5 [1, 2, 3]
test_elemSingle = assertTrue $ elem' 'a' "a"
test_elemEmptyList = assertFalse $ elem' 1 []

-- 1.3
test_or = assertTrue $ or' [True, False]
test_orFalsy = assertFalse $ or' [False, False, False]
test_orSingleElement = assertTrue $ or' [True]
test_orSingleElementFalsy = assertFalse $ or' [False]
test_orEmptyList = assertFalse $ or' []

-- 1.4
test_map = assertEqual mx (map' id mx) where mx = [1,2,3]
test_mapFunc = assertEqual [1, 4, 9] (map' (^2) [1, 2, 3])
test_mapSingle = assertEqual [False] (map' not [True])
test_mapEmptyList = assertEqual [] (map' not [])

-- 1.5
test_filter = assertEqual [True, True] (filter' (== True) [True, False, True, False])
test_filterAll = assertEqual [] (filter' even [1, 3, 5])
test_filterNone = assertEqual mx (filter' (== 2) mx) where mx = [2, 2, 2]
test_filterSingle = assertEqual mx (filter' odd mx) where mx = [5]

-- 1.6
test_plusplus = assertEqual [1, 2] (plusplus [1] [2])
test_plusplusSizes = assertEqual "Alan Turing" (plusplus "Alan " "Turing")
test_plusplusEmptyLeft = assertEqual mx (plusplus [] mx) where mx = [True, False]
test_plusplusEmptyRight = assertEqual mx (plusplus mx "") where mx = "test"
test_plusplusBothEmpty = assertEqual mx (plusplus mx mx) where mx = [] :: [Bool]

-- 1.7 and Exercise 2
test_reverse = assertEqual [1, 2, 3] (reverse' [3, 2, 1])
test_reverse' = assertEqual [1, 2, 3] (reverse'' [3, 2, 1])
test_reverseEmpty = assertEqual [] (reverse' [] :: [Bool])
test_reverseEmpty' = assertEqual [] (reverse'' [] :: [Bool])
test_reverseSingle = assertEqual mx (reverse' mx) where mx = "a"
test_reverseSingle' = assertEqual mx (reverse'' mx) where mx = "a"