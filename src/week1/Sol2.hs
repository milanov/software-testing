module Sol2 where
import GS
import TAMO


-- Exercise 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p lst = length (filter p lst) == 1


-- Exercise 2.52
parity :: [Bool] -> Bool
parity lst = even (length (filter (== True) lst))


-- Exercise 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p = parity . map p