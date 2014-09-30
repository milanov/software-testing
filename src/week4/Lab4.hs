{-# LANGUAGE FlexibleInstances #-}

module Lab4 where

import SetOrd
import Data.List(nub, sort, (\\))
import Control.Monad
import System.Random
import Test.QuickCheck

-- Exercise 1
-- 1) We needed some time to fully understand Question 8 from the Workshop

-- Exercise 2
-- Time spent:
--    hspec: 1 hour, mostly to change the project dependencies to hspec
--      and convert the test suites (previously HTF).
--    quickcheck: we've been using it since week two so no additional time
--      was spent on it


-- Exercise 3
-- Time spent: 30 minutes
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomSet :: IO (Set Int)
getRandomSet = do
                 rndInts <- replicateM 50 (getRandomInt 100)
                 let sortedUniqueInts = sort $ nub rndInts
                 return $ Set sortedUniqueInts

instance Arbitrary (Set Int) where
  arbitrary = do
                rndInts <- replicateM 50 (choose(0, 100))
                let sortedUniqueInts = sort $ nub rndInts
                return $ Set sortedUniqueInts

-- Exercise 4
-- Time spent: 40 minutes
-- Test suite located in tests/week4/Lab4Spec#Exercise 4 contains
-- both unit tests and quickcheck property tests.

-- unionSet is defined in SetOrd.hs

intersectionSet :: Ord a => Set a -> Set a -> Set a
intersectionSet (Set s) p = Set $ filter ((flip inSet) p) s

differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet (Set s) p = Set $ filter (not . ((flip inSet) p)) s


-- Exercise 5
-- Time spent: 1 hour
type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r | length r == length nextr = r
         | otherwise = trClos nextr
         where nextr = nub (sort (r ++ (r @@ r)))


-- Exercise 6 and 7
-- Time spent: 20 minutes
-- Hspec specification located in tests/week4/Lab4Spec#Exercise 6 containts
-- both unit tests and quickcheck property tests. The latter use simple
-- properties and the one given below.

-- A check for transitivity of R, influenced by "The Haskell Road", p. 179
-- Used for automatic testing of {trClos}.
transR :: Ord a => Rel a -> Bool
transR [] = True
transR rel = and [ trans el rel | el <- rel ]
  where trans (x,y) r = and [ elem (x,v) r | (u,v) <- r, u == y ]



{-
Exercise 8

This is the famous Babylonian method of successive approximations. It says that whenever
we have a guess y for the value of the square root of a number x, we can perform a simple
manipulation to get a better guess (one closer to the actual square root) by averaging y
with x/y.

For example, we can compute the square root of 2 as follows:
Guess    |    Quotient        |          Averate
  1             2/1=2                  (2+1)/2=1.5
  1.5         2/1.5=1.(3)          (1.(3)+1.5)/2=1.4167
  1.4167      .........                ...........
  ......
This way we get a convergent sequence {A1,A2,..,An,..} for which lim{Ai} -> sqrt(S) as
i approaches infinity.

In Haskell, however, this process does not continue to infinity since we reach a certain
point the An and An+1 become equal (since Haskell's Double uses the standard double-precision arithmetic)
and the function terminates.
-}
fp :: Eq a => (a -> a) -> a -> a
fp f = \ x -> if x == f x then x else fp f (f x)

