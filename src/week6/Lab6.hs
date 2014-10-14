module Lab6 where

import Data.List
import System.Random
import Week6
import Control.Monad
import Control.Monad.Loops

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [3..], 
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

exMK :: Integer -> Integer -> Integer -> Integer
exMK base 0 m = 1
exMK base e m | odd e = rem ((rem base m) * (exMK base (e - 1) m)) m
              | otherwise = rem ((exMK base (div e 2) m) ^ 2) m

-- Exercise 2

-- Exercise 3
composite :: [Integer]
composite = filter (\ x -> not $ elem x (takeWhile (<= x) primes)) [2..]

-- Exercise 4
testPrimesF = [ firstM (\ x -> do
                              isPr <- primeF k x
                              return isPr) composite | k <- [1..5]]

-- Exercise 5
-- Carmichael are composite numbers such that if n is a carmichael number
-- then for all b such that 1 < b < n 
-- we have b^n = n*k + b where k is an integer number.
-- So carmichael numbers satisfy the same condition as the prime numbers from Fermat's little theorem.
-- So even though they are composite, the carmichael numbers still pass the Fermat's primality.
-- We test the Fermat's primality check with a certain number of carmichael numbers. 
-- With a high probability the test passes.
-- The test doesn't pass every time, because the Fermat's primality check chooses randomly
-- 'a' from the interval from 1 to n-1 (n-2) and if 'a' and 'n'
-- are not co-prime then we can't divide each side of (a^n `mod` n = a) with a
testCarmichaelNumbers :: IO Bool
testCarmichaelNumbers = allM (\ x -> do
                                    isPr <- primeF 5 x
                                    return isPr) (take 1000 carmichael)

-- Exercise 7
findMarsennePrimes :: IO [(Integer, Integer)]
findMarsennePrimes = filterM (\ (power, potentialPrime) -> do
                                    isPr <- primeMR 5 potentialPrime
                                    return isPr) $ map (\ x -> (x, 2^x - 1)) $ take 500 $ drop 50 primes

-- Еxercise 8
-- findPairs :: IO [(Integer, Integer)]

bitLength :: Integer -> Int
bitLength 0 = 0
bitLength x = 1 + bitLength (div x 2)

groupByBitLength :: [[Integer]]
groupByBitLength = groupBy ( \ x y -> bitLength x == bitLength y) $ take 200 primes

