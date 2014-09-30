{-# LANGUAGE FlexibleInstances #-}

module Lab4 where

import SetOrd
import Data.List(nub)
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
                 return $ Set uniqueInts

instance Arbitrary (Set Int) where
  arbitrary = do
                rndInts <- replicateM 50 (choose(0, 100))
                let uniqueInts = nub rndInts
                return $ Set uniqueInts


-- Exercise 8
fp :: Eq a => (a -> a) -> a -> a
fp f = \ x -> if x == f x then x else fp f (f x)

