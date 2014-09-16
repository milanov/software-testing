module Lab2 where

import Data.List
import System.Random


-- Exercise 1
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | a + b <= c || any (<= 0) [x, y, z] = NoTriangle
               | a == c = Equilateral
               | a*a+ b*b == c*c = Rectangular
               | a == b || b == c = Isosceles
               | otherwise = Other
               where a:b:c:[] = sort [x, y, z]
-- For the testing procedure we've used HUnit unit tests located at src/tests/week2/TestLab2.hs (Exercise 1).
-- 


-- Exercise 2
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation l1 l2 = length l1 == length l2 && l1 \\ l2 == []


-- Exercise 3



-- Exercise 4
perms :: Eq a => [a] -> [[a]]
perms [x] = [[x]]
perms l = [ y | x <- l, y <- map (x:) (perms (delete x l))]


-- Exercise 5
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement l1 l2 = isPermutation l1 l2 && all (== False) (zipWith (==) l1 l2)


-- Exercise 6
deran :: Integer -> [[Integer]]
deran n = filter (isDerangement l) (perms l) where l = [0..n-1]


-- Exercise 7



-- Exercise 8
-- The correctness of this function (arbitDeran) does not follow from tests of the other
-- functions. This is because it does not uses any of them. Therefore a separate testing
-- suite is needed.
pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

arbitDeran' :: Int -> [Int] -> IO [Int]
arbitDeran' _ [_] = do
                      return []
arbitDeran' inx (x:y:[]) = do
              if (inx + 1) == y
                then return [y, x]
                else return [x, y]
arbitDeran' inx lst = do
  rnd <- pick (delete inx lst)
  rest <- arbitDeran' (inx + 1) (delete rnd lst)
  return (rnd:rest)

arbitDeran :: Int -> IO [Int]
arbitDeran n = arbitDeran' 0 [0..n-1]


-- Exercise 9
numberOfDerangement :: Int -> Int
numberOfDerangement 1 = 0
numberOfDerangement n = ((-1) ^ n) + n * (numberOfDerangement (n - 1))