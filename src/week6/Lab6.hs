module Lab6 where

import Data.List
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
exMWiki :: Integer -> Integer -> Integer -> Integer
exMWiki num expon modul = exMHelper num expon modul 1
  where exMHelper :: Integer -> Integer -> Integer -> Integer -> Integer
        exMHelper _ 0 _ res = res
        exMHelper num expon modul res = exMHelper (num * num `mod` modul) (expon `div` 2) modul nRes
          where nRes = if odd expon then (res * num `mod` modul) else res

exMReal :: Integer -> Integer -> Integer -> Integer
exMReal base 0 _ = 1
exMReal base e m | odd e = rem ((rem base m) * (exMReal base (e - 1) m)) m
                 | otherwise = rem ((exMReal base (div e 2) m) ^ 2) m


-- Exercise 2
-- Benchmarks of the two exponention functions above and the native implementation (^) are generated
-- in src/week6/expBenchmarks.html. They are done in a reliable manner using criterion - a benchmarking
-- library for Haskell. The benchmark is done with relatively small numbers because of the slow native
-- implementation (95.60 ms for the native function vs less that 0.01 ms for the other two in the example).


-- Exercise 3
composites :: [Integer]
composites = compositesHelper [2..] primes
  where compositesHelper (c:cx) pl@(p:px) | c == p = compositesHelper cx px
                                          | otherwise = c:compositesHelper cx pl


-- Exercise 4
-- Least composite numbers that were found using the {primeF} check:
--   k = 1: 4
--   k = 2: 4
--   k = 3: 91
--   k = 4: 1105
--   k = 5: 1105
-- The more k is increased the bigger the returned numbers become because more
-- checks are done in {primeF} and is therefore harder for a composite number to
-- "fool" the check.
testPrimeF = [firstM (primeF k) composites | k <- [1..5]]


-- Exercise 5
-- A Carmichael number is a composite number n which satisfies
-- b^n = n*k + b, k - integer
-- for all integers 1<b<n for which b and n are relatively prime.
-- So Carmichael numbers satisfy the same condition as the prime numbers from Fermat's
-- little theorem. This is why they could pass Fermat's primality check even though they 
-- are composite.
-- We test the check with a certain number of Carmichael numbers (1000 in the test below).
-- The test doesn't pass every time, because Fermat's primality check chooses a random
-- 'a' from [1..n-1]. If 'a' and 'n' are not co-prime then we can't divide each side of
-- (a^n `mod` n = a) with 'a'. Then the equation 'exM a (n-1) n == 1' doesn't hold and 
-- the check returns false.
testPrimeFCarmichael :: IO Bool
testPrimeFCarmichael = allM (primeF 10) (take 1000 carmichael)


-- Exercise 6
-- Using the test below was found that unlike in the original Fermat's primality check,
-- none of the first 1000 Carmichael numbers "fool" the Miller-Rabin test (for k = 10).
-- This test improves things by using the fact that there are no nontrival square roots
-- of unity modulo a prime.
-- It turns out that for any composite n (including Carmichael numbers), the probability n
-- passes the Miller-Rabin test is at most 1 /4. On average it is significantly less. Thus 
-- the probability n passes several runs decreases exponentially.
testPrimeMR :: IO Bool
testPrimeMR = anyM (primeMR 10) (take 1000 carmichael)



-- Exercise 7
-- The function below returns [521,607,1279,2203,2281,3217] every time it was tested.
-- Consulting Wikipedia reveals that these are indeed known integers n, such that
-- 2^n - 1 is prime (i.e. Mersenne primes).
findMarsennePrimes :: IO [Integer]
findMarsennePrimes = filterM (\x -> primeMR 10 (2^x - 1)) (take 500 $ drop 50 primes)


-- Еxercise 8
bitLength :: Integer -> Int
bitLength 0 = 0
bitLength x = 1 + bitLength (div x 2)

groupByBitLength :: [[Integer]]
groupByBitLength = groupBy ( \ x y -> bitLength x == bitLength y) $ take 200 primes
