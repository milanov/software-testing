module Sol1 where
import GS

-- Exercise 1.4
-- No, because if `k^2` equals `n` then `k` is a divisor of `n` and the previous guarded equation would have been executed.


-- Exercise 1.6
-- rem :: Integer a => a -> a -> a


-- Exercise 1.7
-- `:t divides 5` produces a function of type `Integer -> Bool`. The original function has a type 
-- `Integer -> Integer -> Bool`. When we pass only the first argument we get a function of type
-- `Integer -> Bool`.
-- `:t divides 5 7` produces `Bool` because the `divides` function takes two arguments and by passing
-- 5 and 7 we get a result of the same type as the return type of the function `divides`.


-- Exercise 1.9
maxInt :: [Int] -> Int
maxInt [] = error "Can't find maximum of empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)


-- Exercise 1.10
removeFstElem :: Eq a => a -> [a] -> [a]
removeFstElem _ [] = []
removeFstElem el (x:xs) | el == x = xs
                        | otherwise = x : removeFstElem el xs

removeFst :: Int -> [Int] -> [Int]
removeFst = removeFstElem


-- Exercise 1.13
count :: Char -> String -> Int
count givenChar givenString = length $ filter ( == givenChar) givenString


-- Exercise 1.14
blowup :: String -> String
blowup = concat . zipWith replicate [1..]


-- Exercise 1.15
minElem :: Ord a => [a] -> a
minElem [] = error "Can't find minimum of empty list"
minElem (x:xs) = foldl min x xs

srtString :: [String] -> [String]
srtString [] = []
srtString xs = m : (srtString (removeFstElem m xs)) where m = minElem xs


-- Exercise 1.17
substring :: String -> String -> Bool
substring _ [] = False
substring needleStr (x:xs) = prefix needleStr (x:xs) || substring needleStr xs


-- Exercise 1.18
-- 1) :t ["needle", "haystack"] :: [String]
-- 2) :t (True, "True") :: (Bool,String)
-- 3) :t [(True, "True")] :: [(Bool,String)]
-- 4) :t ([True, False], "True") :: ([Bool],String)
-- 5) :t not :: Bool -> Bool

-- Exercise 1.19
-- 1) :t head :: [a] -> a Takes a list and returns the first element of it
-- 2) :t last :: [a] -> a Takes a list and returns the last element of it
-- 3) :t init :: [a] -> [a] Takes a list and return a new list containing all the elements from the original except for the last one
-- 4) :t fst :: (a, b) -> a Takes a tuple containing two elements and returns the first one
-- 5) :t (++) :: [a] -> [a] -> [a] Returns a concatenation of the two given lists
-- 6) :t flip :: (a -> b -> c) -> b -> a -> c Evaluates the given function but flips the order of its arguments
-- 7) :t flip (++) :: [a] -> [a] -> [a] Concatenates two lists but in the returned list the elements of the second list come first


-- Exercise 1.20
lengths :: [[a]] -> [Int]
lengths = map length


-- Exercise 1.21
sumLengths :: [[a]] -> Int
sumLengths = sum . lengths


-- Exercise 1.24
-- Nothing, the change makes no difference. Its only syntactic sugar for leaving out arguments if
-- if they are in the same on the left and right sides of the definition.