module Lab3 where

import Week3
import Test.QuickCheck

-- Exercise 1
-- Time spent: Implementation - 10 minutes
--             Testing        - 20 minutes
-- The tests are both unit and using quickcheck (src/test/week3/TestLab3#Exercise 1)
contradiction :: Form -> Bool
contradiction f = none (\ v -> eval v f) (allVals f) where none pr lst = not(or $ map pr lst)

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)


-- Exercise 2
-- Time spent: Implementation - 3 hours
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg(Prop x)) = Neg(Prop x)
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj []) = Dsj []
cnf (Dsj fs) = foldr1 dist (map cnf fs)
cnf _ = error "Invalid formula"

dist :: Form -> Form -> Form
dist (Cnj fs) x = Cnj (map ((flip dist) x) fs)
dist x (Cnj fs) = Cnj (map (dist x) fs)
dist x y = Dsj [x, y]

isDsj :: Form -> Bool
isDsj (Dsj _) = True
isDsj _ = False

isCnj :: Form -> Bool
isCnj (Cnj _) = True
isCnj _ = False

flatten :: Form -> Form
flatten (Cnj fs) = Cnj (map flatten (flattenBy (Cnj fs) isCnj))
flatten (Dsj fs) = Dsj (flattenBy (Dsj fs) isDsj)
flatten f = f

flattenBy :: Form -> (Form -> Bool) -> [Form]
flattenBy fm pr = case fm of
             (Cnj fs)    -> concatBy pr fs
             (Dsj fs)    -> concatBy pr fs
             _           -> [fm]
  where concatBy prd fs = concatMap (\f -> if prd f then flattenBy f prd else [f]) fs

toCnf :: Form -> Form
toCnf = flatten . cnf . nnf . arrowfree


-- Exercise 3
-- Time spent: Implementation - 2 hours
--             Unit testing   - 10 minutes
-- The actual testing of the `toCnf` function is done using the generator
-- below (with quickcheck) in src/test/week3/TestLab3#Exercise 3 - toCnf. 
isLiteral :: Form -> Bool
isLiteral (Prop _) = True
isLiteral (Neg(Prop _)) = True
isLiteral _ = False

isDisjunctionOfLiterals :: Form -> Bool
isDisjunctionOfLiterals (Dsj fs) = all isLiteral fs
isDisjunctionOfLiterals f = isLiteral f

isConjunctionOfDisjunctions :: Form -> Bool
isConjunctionOfDisjunctions (Cnj fs) = all isDisjunctionOfLiterals fs
isConjunctionOfDisjunctions _ = False

isInCnf :: Form -> Bool
isInCnf f = isLiteral f || isDisjunctionOfLiterals f || isConjunctionOfDisjunctions f

-- arbitrary form generator used in the quickcheck tests
instance Arbitrary Form where
  arbitrary = do
                d <- choose(0, 3)
                f <- getRandomFormula d
                return f

getRandomFormula :: Int -> Gen Form 
getRandomFormula 0 = do m <- choose(0, 20)
                        return $ Prop m

getRandomFormula d = do n <- choose(0::Int, 5::Int)
                        case n of
                          0 -> do m <- choose(0, 20)
                                  return (Prop m)
                          1 -> do f <- getRandomFormula (d-1)
                                  return (Neg f)
                          2 -> do m  <- choose(0, 5)
                                  fs <- getListOfRandomFormulas (d-1) m
                                  return (Cnj fs)
                          3 -> do m  <- choose(0, 5)
                                  fs <- getListOfRandomFormulas (d-1) m
                                  return (Dsj fs)
                          4 -> do f <- getRandomFormula (d-1)
                                  g <- getRandomFormula (d-1)
                                  return (Impl f g)
                          5 -> do f <- getRandomFormula (d-1)
                                  g <- getRandomFormula (d-1)
                                  return (Equiv f g)

getListOfRandomFormulas :: Int -> Int -> Gen [Form]
getListOfRandomFormulas _ 0 = return []
getListOfRandomFormulas d n = do f  <- getRandomFormula d
                                 fs <- getListOfRandomFormulas d (n-1) 
                                 return (f:fs)

-- Exercise 4
-- Time spent: Implementation - 10 minutes
--             Testing        - 30 minutes
-- The tests are done using quickcheck (src/test/week3/TestLab3#Exercise 4)
type Clause = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg(Prop x)) = [[-x]]
cnf2cls (Dsj fs) = [concat $ concatMap cnf2cls fs]
cnf2cls (Cnj fs) = concatMap cnf2cls fs
cnf2cls _ = error "Invalid formula"
