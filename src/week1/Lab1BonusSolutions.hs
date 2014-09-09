module Lab1BonusSolutions where

import Lab1Bonus

-- Exercise 1

-- 1.1
length' :: [a] -> Int
length' = foldr (\_ len -> len + 1) 0

-- 1.2
elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\el res -> res || el == x) False

-- 1.3
or' :: [Bool] -> Bool
or' = foldr (||) False

-- 1.4
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).f) []

-- 1.5
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- 1.6
plusplus :: [a] -> [a] -> [a]
plusplus = flip $ foldr (:)

-- 1.7
reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []


-- Exercise 2
reverse'':: [a] -> [a]
reverse'' = foldl (flip (:)) []


-- Exercise 3
-- Only `foldr` can work on infinite lists. The explanation comes from the way recursion is done in `foldr`/`foldl`:
-- foldl :: f (... (f ( f (f z x1) x2) x3) ...) xn
-- foldr :: f x1 (f x2 (f x3 (...(f xn z) ...)))
-- For `foldl` we need to get to the end of the list to make the most outer call. In case of `foldr` we need the first element
-- of the list and the result of processing the rest of the list with `foldr`, which may not be needed if the function passed
-- to `foldr` is lazy to its second argument.


-- Exercise 4
signOne, signTwo :: (Creature,Creature) -> Bool
signOne (x,y) = x == Lady || y == Lady
signTwo (x,_) = x == Tiger

solution4 :: [(Creature,Creature)]
solution4 = 
 [ (x,y) | x <- [Lady,Tiger], 
           y <- [Lady,Tiger], 
           signOne (x,y) == signTwo (x,y) ]
-- Solution: [(Tiger,Lady)]


-- Exercise 5
johnSays, billSays :: (Islander,Islander) -> Bool
johnSays (x,y) = x == y
billSays (x,y) = x /= y

solution5 :: [(Islander,Islander)]
solution5 = [(x,y) | x <- [Knight,Knave], 
                     y <- [Knight,Knave],
                     (johnSays (x,y) == (x == Knight)) &&
                     (billSays (x,y) == (y == Knight))]
-- Solution: [(Knave,Knight)]

-- Exercise 6



-- Exercise 7
solution :: [Boy]
solution = [ boy | boy <- boys,
                   let count el lst = length $ filter ( == el) lst,
                   let mx = map ($ boy) declarations,
                   count True mx == 3 && count False mx == 2]
-- Solution: [Jack]

honest :: [[Boy]]
honest = [[boy | (decl, boy) <- table, decl guilty] | guilty <- solution]
-- Solution: [[Matthew,Peter,Carl]]