{-# LANGUAGE FlexibleInstances #-}

module Lab4 where

import SetOrd
import Data.List(nub, intersect, sort)
import Control.Monad
import System.Random
import Test.QuickCheck


-- Exercise 3
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomSet :: IO (Set Int)
getRandomSet = do
                 rndInts <- replicateM 50 (getRandomInt 100)
                 let uniqueInts = nub rndInts
                 return $ Set (sort uniqueInts)

instance Arbitrary (Set Int) where
  arbitrary = do
                rndInts <- replicateM 50 (choose(0, 100))
                let uniqueInts = nub rndInts
                return $ Set (sort uniqueInts)

-- Exercise 4
intersectionSet :: Eq a => Set a -> Set a -> Set a
intersectionSet (Set s) (Set p) = Set (intersect s p)

differenceSet :: Eq a => Set a -> Set a -> Set a
differenceSet (Set s) (Set p) = Set lst where lst = filter (not . ((flip elem) p)) s


-- Exercise 5
type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r | length r == length nextr = r
         | otherwise = trClos nextr
         where nextr = nub (sort (r ++ (r @@ r)))

-- A check for transitivity of R, influenced by "The Haskell Road", p. 179
-- Used for automatic testing of {trClos}.
transR :: Ord a => Rel a -> Bool
transR [] = True
transR rel = and [ trans el rel | el <- rel ]
  where trans (x,y) r = and [ elem (x,v) r | (u,v) <- r, u == y ]


-- Exercise 8
fp :: Eq a => (a -> a) -> a -> a
fp f = \ x -> if x == f x then x else fp f (f x)

