module Lab5 where

import Data.List
import Week5

-- Exercise 1
-- Hspec specifications to some of the functions from the Week5 module are located
-- in src/tests/week5/Lab5Spec.hs. Yes, it is possible to use Quickcheck. Sudokues
-- are very well logically defined and have properties, which can be tested using 
-- QuickCheck. However, we're not using it since this would require us to rewrite
-- all the sudoku random generation using Gen and not IO.


-- Exercise 2
-- Time spent: 20 minutes
minimal :: Node -> Bool
minimal n = uniqueSol n && minimalHints n

minimalHints :: Node -> Bool
minimalHints n@(s, _) = and [not $ uniqueSol (eraseN n pos) | pos <- filledPositions s]

-- Exercise 3
-- Yes, it is possible to generate sudoku puzzles with four empty blocks. For proof we can
-- change the {sublistsOfSubgrids} function to call `sublistsOf 4` instead of `sublistsOf 3`
-- and we'll see that such puzzles are quickly generated.
-- It is theoretically impossible to define puzzles with six or more empty blocks, since then
-- there will always be three empty block in the same row/column, which makes the puzzle
-- undeterministic (with more than one solution).
-- The semi-formal proof for why a deterministic Sudoku problem with five empty blocks cannot
-- exist is given here: http://puzzling.stackexchange.com/a/319. Another way of checking this
-- is again if we run the {sublistsOfSubgrids} with `sublistsOf 5` and then wait for all the
-- 6,670,903,752,021,072,936,960 solutions to be checked ;)
positionsInSubgrid :: (Row, Column) -> [(Row, Column)]
positionsInSubgrid (r, c) = [ (r',c') | r' <- bl r, c' <- bl c ]

sublistsOf :: Int -> [a] -> [[a]]
sublistsOf n = filter ((== n) . length) . subsequences

sublistsOfSubgrids :: IO [[(Row, Column)]]
sublistsOfSubgrids = randomize $ sublistsOf 3 [ (r, c) | r <- [1,4,7], c <- [1,4,7]]

eraseSubgrid :: Node -> (Row, Column) -> Node
eraseSubgrid n rc = eraseSubgridHelper n (positionsInSubgrid rc)
  where eraseSubgridHelper :: Node -> [(Row, Column)] -> Node
        eraseSubgridHelper nd lst = if null lst
                                    then nd
                                    else eraseSubgridHelper (eraseN nd (head lst)) (tail lst)

eraseSubgrids :: Node -> [(Row, Column)] -> Node
eraseSubgrids n [] = n
eraseSubgrids n (x:xs) = eraseSubgrids (eraseSubgrid n x) xs

genSudoku3Empty :: IO Node
genSudoku3Empty = genSudoku3EmptyHelper (solveNs [emptyN])

genSudoku3EmptyHelper :: [Node] -> IO Node
genSudoku3EmptyHelper [] = error "No node found"
genSudoku3EmptyHelper (x:xs) = do
                                subLists <- sublistsOfSubgrids
                                let unique = find uniqueSol (map (eraseSubgrids x) subLists)
                                case unique of
                                  Just n  -> genProblem n
                                  Nothing -> genSudoku3EmptyHelper xs

-- Exercise 4
blocksNrc :: [[Int]]
blocksNrc = [[2..4],[6..8]]

blNrc :: Int -> [Int]
blNrc x = concat $ filter (elem x) blocksNrc

subGridNrc :: Sudoku -> (Row,Column) -> [Value]
subGridNrc s (r,c) = [ s (r',c') | r' <- blNrc r, c' <- blNrc c ]

freeInSubgridNrc :: Sudoku -> (Row,Column) -> [Value]
freeInSubgridNrc s (r,c) = freeInSeq (subGridNrc s (r,c))

freeAtPos' :: Sudoku -> (Row,Column) -> [Value]
freeAtPos' s (r,c) = (freeInRow s r)
                    `intersect` (freeInColumn s c)
                    `intersect` (freeInSubgrid s (r,c))
                    `intersect` (freeInSubgridNrc s (r,c))

subgridInjectiveNrc :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveNrc s (r,c) = injective vs where vs = filter (/= 0) (subGridNrc s (r,c))

consistent' :: Sudoku -> Bool
consistent' s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subgridInjectiveNrc s (r,c) | r <- [2,6], c <- [2,6]]

sameblockNrc :: (Row,Column) -> (Row,Column) -> Bool
sameblockNrc (r,c) (x,y) = blNrc r == blNrc x && blNrc c == blNrc y

prune' :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune' _ [] = []
prune' (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | sameblock (r,c) (x,y) = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | sameblockNrc (r,c) (x,y) = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | otherwise = (x,y,zs) : prune' (r,c,v) rest


-- below are functions which are the same as their counterparts in Week5, but
-- they are modified to use the new NRC constraints
constraints' :: Sudoku -> [Constraint] 
constraints' s = sortBy length3rd 
    [(r,c, freeAtPos' s (r,c)) | (r,c) <- openPositions s ]

initNode' :: Grid -> [Node]
initNode' gr = let s = grid2sud gr in 
              if (not . consistent') s then [] 
              else [(s, constraints' s)]

extendNode' :: Node -> Constraint -> [Node]
extendNode' (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v), sortBy length3rd $ prune' (r,c,v) constraints) | v <- vs ]

succNode' :: Node -> [Node]
succNode' (s,[]) = []
succNode' (s,p:ps) = extendNode' (s,ps) p

solveNs' :: [Node] -> [Node]
solveNs' = search succNode' solved 

solveShowNs' :: [Node] -> IO()
solveShowNs' = sequence_ . fmap showNode . solveNs'

solveAndShow' :: Grid -> IO()
solveAndShow' gr = solveShowNs' (initNode' gr)

nrc :: [[Int]]
nrc = [[0,0,0,3,0,0,0,0,0],
       [0,0,0,7,0,0,3,0,0],
       [2,0,0,0,0,0,0,0,8],
       [0,0,6,0,0,5,0,0,0],
       [0,9,1,6,0,0,0,0,0],
       [3,0,0,0,7,1,2,0,0],
       [0,0,0,0,0,0,0,3,1],
       [0,8,0,0,4,0,0,0,0],
       [0,0,2,0,0,0,0,0,0]]


-- Exercise 5
rsuccNode' :: Node -> IO [Node]
rsuccNode' (s,cs) = 
  do xs <- getRandomCnstr cs
     if null xs 
        then return []
        else return (extendNode' (s,cs\\xs) (head xs))

rsolveNs' :: [Node] -> IO [Node]
rsolveNs' ns = rsearch rsuccNode' solved (return ns)
