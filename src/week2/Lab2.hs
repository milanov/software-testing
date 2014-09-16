module Lab2 where

import Data.List
import System.Random


-- Exercise 1
-- For the testing procedure we've used HUnit unit tests located at src/tests/week2/TestLab2.hs (Exercise 1).
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | a + b <= c || any (<= 0) [x, y, z] = NoTriangle
               | a == c = Equilateral
               | a*a+ b*b == c*c = Rectangular
               | a == b || b == c = Isosceles
               | otherwise = Other
               where a:b:c:[] = sort [x, y, z]


-- Exercise 2
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation l1 l2 = length l1 == length l2 && l1 \\ l2 == []


-- Exercise 3
-- For the testing procedure we've used HUnit unit tests located at src/tests/week2/TestLab2.hs (Exercise 2).
-- Some testable properties are:
-- * the lengths of the two lists should be equal
-- * there isn't an element from one list which is not in the other list
-- * each element from the first list to have the same number of appearances as in the second list
testIsPermutation :: Eq a => [a] -> [a] -> Bool
testIsPermutation l1 l2 = length l1 == length l2 &&
                          (all (\x -> elem x l2) l1 && all (\x -> elem x l1) l2) &&
                          all (\x -> count x l1 == count x l2) l1
                          where count x l = length (filter (==x) l)



-- Exercise 4
-- The number of permutations of a list without duplicates is n!
-- We can write a testable property for the perms function which
-- checks whether the count of all returned permutations is equal
-- to n factorial.
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
-- Some testable properties are:
-- * the lengths of the two lists should be equal
-- * there isn't an element from one list which is not in the other list
-- * each element from the first list to have the same number of appearances as in the second list
-- * for every index 0..n-1 the corresponding values in the lists to be different
testIsDerangement :: Eq a => [a] -> [a] -> Bool
testIsDerangement l1 l2 = length l1 == length l2 &&
                          (all (\x -> elem x l2) l1 && all (\x -> elem x l1) l2) &&
                          all (\x -> count x l1 == count x l2) l1 &&
                          all (\x -> l1 !! x /= l2 !! x) [0..(length l1 - 1)]
                          where count x l = length (filter (==x) l) 



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