module Lab1Bonus

where 

data Creature = Lady | Tiger 
     deriving (Eq,Show)

sign1, sign2 :: (Creature,Creature) -> Bool
sign1 (x,y) = x == Lady && y == Tiger
sign2 (x,y) = x /= y

solution1 :: [(Creature,Creature)]
solution1 = 
 [ (x,y) | x <- [Lady,Tiger], 
           y <- [Lady,Tiger], 
           sign1 (x,y) /= sign2 (x,y) ]

data Islander = Knight | Knave deriving (Eq,Show)

john :: (Islander,Islander) -> Bool
john (x,y) = (x,y) == (Knave,Knave)

solution3 :: [(Islander,Islander)]
solution3 = [(x,y) | x <- [Knight,Knave], 
                     y <- [Knight,Knave], 
                     john (x,y) == (x == Knight) ]

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

matthew, peter, jack, arnold, carl :: Boy -> Bool
matthew = \ x -> not (x==Matthew) && not (x==Carl)
peter   = \ x -> x==Matthew || x==Jack
jack    = \ x -> not (matthew x) && not (peter x)
arnold  = \ x -> matthew x /= peter x
carl    = \ x -> not (arnold x)

declarations = [matthew,peter,jack,arnold,carl]
table = zip declarations boys 


-- SOLUTIONS --

-- Exercise 1
-- 1.1
length' :: [a] -> Int
length' = foldr (\_ -> (+1)) 0

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
-- You have three numbers 1 <= x <= y <= z <= 3 and the following conditions:
-- 1) 2*x > z
-- 2) x != z
-- 3) x + y + z < 8
-- Find x, y, z.
solution6 :: [(Int, Int, Int)]
solution6 = [(x, y, z) | x <- [1..3], y <- [1..3], z <- [1..3],
                         2*x > z,
                         x /= z,
                         x + y + z < 8,
                         x <= y, y <= z]


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