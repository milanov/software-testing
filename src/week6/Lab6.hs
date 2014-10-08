module Lab6 where

import Data.List
import System.Random
import Week6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- Exercise 1
exM' :: Integer -> Integer -> Integer -> Integer
exM' num expon modul = exMHelper num expon modul 1
  where exMHelper :: Integer -> Integer -> Integer -> Integer -> Integer
        exMHelper _ 0 _ res = res
        exMHelper num expon modul res = exMHelper (num * num `mod` modul) (expon `div` 2) modul nRes
          where nRes = if odd expon then (res * num `mod` modul) else res

-- Exercise 2
