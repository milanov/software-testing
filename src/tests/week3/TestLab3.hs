{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestLab3 where

import Test.HUnit.Lang
import Test.Framework
import Helper

import Week3
import Lab3
import Data.List


-- Exercise 1
-- contradiction
test_contradictionFalsy = assertFalse $ contradiction f where f = Impl (Prop 1) (Prop 2)
test_contradictionFalsy2 = assertFalse $ contradiction f where f = Neg(Prop 1)
test_contradictionFalsy3 = assertFalse $ contradiction f where f = Cnj [Prop 1, Prop 2, Neg(Prop 3)]

prop_contradiction f = contradiction (Cnj [f, (Neg f)])

-- tautology
test_tautologyFalsy = assertFalse $ tautology f where f = Impl (Prop 1) (Prop 2)
test_tautologyFalsy2 = assertFalse $ tautology f where f = Neg(Prop 1)
test_tautologyFalsy3 = assertFalse $ tautology f where f = Cnj [Prop 1, Prop 2, Neg(Prop 3)]

prop_tautology f = tautology (Dsj [f, (Neg f)])
prop_tautologyImpl f = tautology (Impl f f)
prop_tautologyEquiv f = tautology (Equiv f f)

-- entails
test_entailsFalsy = assertFalse $ entails (Prop 1) (Prop 2)
test_entailsFalsy2 = assertFalse $ entails (Dsj props) (Cnj props) where props = [(Prop 1), (Prop 2)]
test_entailsFalsy3 = assertFalse $ entails (Impl f1 f2) (Equiv f1 f2) where f1 = Prop 1
                                                                            f2 = Prop 2
prop_entailsFromFalse f = entails contrad f where contrad = Cnj [Prop 1, Neg (Prop 1)]
prop_entailsToTrue f = entails f taut where taut = Dsj [Prop 1, Neg (Prop 1)]

-- equiv
test_equivFalsy = assertFalse $ equiv (Prop 1) (Prop 2)
test_equivFalsy2 = assertFalse $ equiv (Cnj props) (Dsj props) where props = [(Prop 1), (Prop 2)]
test_equivFalsy3 = assertFalse $ equiv (Impl f1 f2) (Equiv f1 f2) where f1 = Prop 1
                                                                        f2 = Prop 2
prop_equivSame f = equiv f f


-- Exercise 3
-- isLiteral
test_isLiteralProp = assertTrue $ isLiteral (Prop 1)
test_isLiteralNegProp = assertTrue $ isLiteral (Neg(Prop 1))
test_isLiteralCnj = assertFalse $ isLiteral (Cnj [(Prop 1), (Prop 2)])
test_isLiteralDsj = assertFalse $ isLiteral (Dsj [(Prop 1), (Prop 2)])
test_isLiteralImpl = assertFalse $ isLiteral (Impl (Prop 1) (Prop 2))
test_isLiteralEquiv = assertFalse $ isLiteral (Equiv (Prop 1) (Prop 2))

-- isDisjunctionOfLiterals
test_isDisjunctionOfLiteralsProp = assertTrue $ isDisjunctionOfLiterals (Prop 1)
test_isDisjunctionOfLiteralsNegProp = assertTrue $ isDisjunctionOfLiterals (Neg(Prop 1))
test_isDisjunctionOfLiteralsDsjTruth = assertTrue $ isDisjunctionOfLiterals (Dsj [(Prop 1), Neg(Prop 2)])
test_isDisjunctionOfLiteralsDsjFalsy = assertFalse $ isDisjunctionOfLiterals (Dsj [(Prop 1), Impl (Prop 1) (Prop 2)])
test_isDisjunctionOfLiteralsCnj = assertFalse $ isDisjunctionOfLiterals (Cnj [(Prop 1), (Prop 2)])
test_isDisjunctionOfLiteralsImpl = assertFalse $ isDisjunctionOfLiterals (Impl (Prop 1) (Prop 2))
test_isDisjunctionOfLiteralsEquiv = assertFalse $ isDisjunctionOfLiterals (Equiv (Prop 1) (Prop 2))

-- isConjunctionOfDisjunctions
test_isConjunctionOfDisjunctionsProp = assertFalse $ isConjunctionOfDisjunctions (Prop 1)
test_isConjunctionOfDisjunctionsNegProp = assertFalse $ isConjunctionOfDisjunctions (Neg (Prop 1))
test_isConjunctionOfDisjunctionsCnjProps = assertTrue $ isConjunctionOfDisjunctions (Cnj [(Prop 1), Neg(Prop 2)])
test_isConjunctionOfDisjunctionsCnjDsjs = assertTrue $ isConjunctionOfDisjunctions (Cnj [(Dsj [(Prop 1), Neg(Prop 1)]), (Prop 1)])
test_isConjunctionOfDisjunctionsCnjDsjsNested = assertFalse $ isConjunctionOfDisjunctions (Cnj [(Dsj [Dsj [(Prop 1)]]), (Prop 1)])
test_isConjunctionOfDisjunctionsCnjCnjsNested = assertFalse $ isConjunctionOfDisjunctions (Cnj [(Cnj [(Prop 1)]), (Prop 1)])
test_isConjunctionOfDisjunctionsCnjDsjsMixed = assertFalse $ isConjunctionOfDisjunctions (Cnj [(Prop 1), (Impl (Prop 1) (Prop 2))])
test_isConjunctionOfDisjunctionsCnjDsjsMixed2 = assertFalse $ isConjunctionOfDisjunctions (Cnj [(Prop 1), (Equiv (Prop 1) (Prop 2))])
test_isConjunctionOfDisjunctionsDsj = assertFalse $ isConjunctionOfDisjunctions (Dsj [(Prop 1), (Prop 2)])
test_isConjunctionOfDisjunctionsEquiv = assertFalse $ isConjunctionOfDisjunctions (Equiv (Prop 1) (Prop 2))
test_isConjunctionOfDisjunctionsImpl = assertFalse $ isConjunctionOfDisjunctions (Impl (Prop 1) (Prop 2))

-- toCnf
prop_toCnf f = isInCnf cnfed && (equiv f cnfed) where cnfed = toCnf f

-- Exercise 4
formulaLength :: Form -> Int
formulaLength (Cnj f) = length f
formulaLength _ = 1

formulaPropsNumber :: Form -> Int
formulaPropsNumber (Prop f) = 1
formulaPropsNumber (Neg(Prop f)) = 1
formulaPropsNumber (Dsj f) = sum (map formulaPropsNumber f)
formulaPropsNumber (Cnj f) = sum (map formulaPropsNumber f)

formulaPropsMultiset :: Form -> [Int]
formulaPropsMultiset (Prop f) = [f]
formulaPropsMultiset (Neg(Prop f)) = [-f]
formulaPropsMultiset (Dsj f) = concatMap formulaPropsMultiset f
formulaPropsMultiset (Cnj f) = concatMap formulaPropsMultiset f

-- test the properties of the cnf2cls function
prop_cnf2cls f = formulaLength cnfed == length c &&
                 formulaPropsNumber cnfed == sum (map length c) &&
                 sort (formulaPropsMultiset cnfed) == sort(concat c)
  where cnfed = toCnf f
        c = cnf2cls cnfed 