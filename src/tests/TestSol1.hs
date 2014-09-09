{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestSol1 where

import Test.HUnit.Lang
import Test.Framework

import Sol1
import Helper

-- Exercise 1.9
test_maxInt = assertEqual mx (maxInt [1, 2, 3, mx]) where mx = 14
test_maxIntSingle = assertEqual mx (maxInt [mx]) where mx = 17
test_maxIntNegative = assertEqual mx (maxInt [-2, -3, mx]) where mx = -1
test_maxIntEmptyList = assertError "Can't find maximum of empty list" ( maxInt [] )


-- Exercise 1.15
test_srtString = assertEqual ["a", "bc", "d"] (srtString ["d", "a", "bc"])
test_srtStringEmpty = assertEqual [] (srtString [])
test_srtStringEqualFirstLetters = assertEqual ["a", "aa", "aaa"] (srtString ["aaa", "aa", "a"])


-- Exercise 1.17
test_substring = assertTrue $ substring "qwe" "1qwerty"
test_substringSame = assertTrue $ substring "qwe" "qwe"
test_substringBeginning = assertTrue $ substring "123" "123test"
test_substringEnd = assertTrue $ substring "123" "test123"
test_substringFalsy = assertFalse $ substring "qwe" "123"
test_substringEmptyLast = assertFalse $ substring "qwe" ""
test_substringEmptyFirst = assertTrue $ substring "" "123"


-- Exercise 1.20
test_lengths = assertEqual [1, 2, 3] (lengths ["a", "bc", "def"])
test_lengthsSingle = assertEqual [2] (lengths [[True, False]])
test_lengthsEmptyLists = assertEqual [0, 1] (lengths [[], [True]])

-- Exercise 1.21
test_sumLengths = assertEqual 6 (sumLengths ["a", "bc", "def"])
test_sumLengthsEmptyList = assertEqual 0 (sumLengths [[]])
test_sumLengthsSingle = assertEqual 2 (sumLengths ["12"])