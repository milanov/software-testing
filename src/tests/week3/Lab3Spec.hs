module Lab3Spec(spec) where

import Test.Hspec
import Test.Hspec.HUnit (fromHUnitTest)
import Test.HUnit
import Test.QuickCheck

import Helper
import Week3
import Lab3
import Data.List


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- Exercise 1
  describe "contradiction" $ do
    fromHUnitTest testSuiteContradiction
    
    it "is always true that `p and !p` is a contradiction" $ property $
      prop_contradiction

  describe "tautology" $ do
    fromHUnitTest testSuiteTautology
    
    it "is always true that `p or !p` is a tautology" $ property $
      prop_tautology
    it "is always true that an implication from `f` to `f` is a tautology" $ property $
      prop_tautologyImpl
    it "is always true that equivalence of the same formula is a tautology" $ property $
      prop_tautologyEquiv

  describe "entails" $ do
    fromHUnitTest testSuiteEntails
    
    it "is always the case that an implication from a falsy formula is true" $ property $
      prop_entailsFromFalse
    it "is always the case that an implication to thruth is true" $ property $
      prop_entailsToTrue

  describe "equiv" $ do
    fromHUnitTest testSuiteEquiv
    
    it "always is true that a formula is equivalent to itself" $ property $
      prop_equivSame

  -- Exercise 3
  describe "isLiteral" $ do
    fromHUnitTest testSuiteIsLiteral

  describe "isDisjunctionOfLiterals" $ do
    fromHUnitTest testSuiteIsDisjunctionOfLiterals

  describe "isConjunctionOfDisjunctions" $ do
    fromHUnitTest testSuiteIsConjunctionOfDisjunctions

  describe "toCnf" $ do
    it ("is always true that after `toCnf` the resulting" ++
        "formula is equivalent to the original and follows the CNF rules") $ property $
      prop_toCnf

  -- Exercise 4
  describe "cnf2cls" $ do
    it ("returns a formula which has the same length " ++
        "and properties (in order) like the original one") $ property $
      prop_cnf2cls


testSuiteContradiction :: Test
testSuiteContradiction = TestList [
  TestLabel "contradictionFalsy" test_contradictionFalsy,
  TestLabel "contradictionFalsy2" test_contradictionFalsy2,
  TestLabel "contradictionFalsy3" test_contradictionFalsy3
  ]

testSuiteTautology :: Test
testSuiteTautology = TestList [
  TestLabel "tautologyFalsy" test_tautologyFalsy,
  TestLabel "tautologyFalsy2" test_tautologyFalsy2,
  TestLabel "tautologyFalsy3" test_tautologyFalsy3
  ]

testSuiteEntails :: Test
testSuiteEntails = TestList [
  TestLabel "entailsFalsy" test_entailsFalsy,
  TestLabel "entailsFalsy2" test_entailsFalsy2,
  TestLabel "entailsFalsy3" test_entailsFalsy3
  ]

testSuiteEquiv :: Test
testSuiteEquiv = TestList [
  TestLabel "equivFalsy" test_equivFalsy,
  TestLabel "equivFalsy2" test_equivFalsy2,
  TestLabel "equivFalsy3" test_equivFalsy3
  ]

testSuiteIsLiteral :: Test
testSuiteIsLiteral = TestList [
  TestLabel "isLiteralProp" test_isLiteralProp,
  TestLabel "isLiteralNegProp" test_isLiteralNegProp,
  TestLabel "isLiteralCnj" test_isLiteralCnj,
  TestLabel "isLiteralDsj" test_isLiteralDsj,
  TestLabel "isLiteralImpl" test_isLiteralImpl,
  TestLabel "isLiteralEquiv" test_isLiteralEquiv
  ]

testSuiteIsDisjunctionOfLiterals :: Test
testSuiteIsDisjunctionOfLiterals = TestList [
  TestLabel "isDisjunctionOfLiteralsProp" test_isDisjunctionOfLiteralsProp,
  TestLabel "isDisjunctionOfLiteralsNegProp" test_isDisjunctionOfLiteralsNegProp,
  TestLabel "isDisjunctionOfLiteralsDsjTruth" test_isDisjunctionOfLiteralsDsjTruth,
  TestLabel "isDisjunctionOfLiteralsDsjFalsy" test_isDisjunctionOfLiteralsDsjFalsy,
  TestLabel "isDisjunctionOfLiteralsCnj" test_isDisjunctionOfLiteralsCnj,
  TestLabel "isDisjunctionOfLiteralsImpl" test_isDisjunctionOfLiteralsImpl,
  TestLabel "isDisjunctionOfLiteralsEquiv" test_isDisjunctionOfLiteralsEquiv
  ]

testSuiteIsConjunctionOfDisjunctions :: Test
testSuiteIsConjunctionOfDisjunctions = TestList [
  TestLabel "isConjunctionOfDisjunctionsProp" test_isConjunctionOfDisjunctionsProp,
  TestLabel "isConjunctionOfDisjunctionsNegProp" test_isConjunctionOfDisjunctionsNegProp,
  TestLabel "isConjunctionOfDisjunctionsCnjProps" test_isConjunctionOfDisjunctionsCnjProps,
  TestLabel "isConjunctionOfDisjunctionsCnjDsjs" test_isConjunctionOfDisjunctionsCnjDsjs,
  TestLabel "isConjunctionOfDisjunctionsCnjDsjsNested" test_isConjunctionOfDisjunctionsCnjDsjsNested,
  TestLabel "isConjunctionOfDisjunctionsCnjCnjsNested" test_isConjunctionOfDisjunctionsCnjCnjsNested,
  TestLabel "isConjunctionOfDisjunctionsCnjDsjsMixed" test_isConjunctionOfDisjunctionsCnjDsjsMixed,
  TestLabel "isConjunctionOfDisjunctionsCnjDsjsMixed2" test_isConjunctionOfDisjunctionsCnjDsjsMixed2,
  TestLabel "isConjunctionOfDisjunctionsDsj" test_isConjunctionOfDisjunctionsDsj,
  TestLabel "isConjunctionOfDisjunctionsEquiv" test_isConjunctionOfDisjunctionsEquiv,
  TestLabel "isConjunctionOfDisjunctionsImpl" test_isConjunctionOfDisjunctionsImpl
  ]


-- Exercise 1
-- contradiction
test_contradictionFalsy = TestCase $ assertFalse $ contradiction f where f = Impl (Prop 1) (Prop 2)
test_contradictionFalsy2 = TestCase $ assertFalse $ contradiction f where f = Neg(Prop 1)
test_contradictionFalsy3 = TestCase $ assertFalse $ contradiction f where f = Cnj [Prop 1, Prop 2, Neg(Prop 3)]

prop_contradiction f = contradiction (Cnj [f, (Neg f)])

-- tautology
test_tautologyFalsy = TestCase $ assertFalse $ tautology f where f = Impl (Prop 1) (Prop 2)
test_tautologyFalsy2 = TestCase $ assertFalse $ tautology f where f = Neg(Prop 1)
test_tautologyFalsy3 = TestCase $ assertFalse $ tautology f where f = Cnj [Prop 1, Prop 2, Neg(Prop 3)]

prop_tautology f = tautology (Dsj [f, (Neg f)])
prop_tautologyImpl f = tautology (Impl f f)
prop_tautologyEquiv f = tautology (Equiv f f)

-- entails
test_entailsFalsy = TestCase $ assertFalse $ entails (Prop 1) (Prop 2)
test_entailsFalsy2 = TestCase $ assertFalse $ entails (Dsj props) (Cnj props) where props = [(Prop 1), (Prop 2)]
test_entailsFalsy3 = TestCase $ assertFalse $ entails (Impl f1 f2) (Equiv f1 f2) where f1 = Prop 1
                                                                                       f2 = Prop 2
prop_entailsFromFalse f = entails contrad f where contrad = Cnj [Prop 1, Neg (Prop 1)]
prop_entailsToTrue f = entails f taut where taut = Dsj [Prop 1, Neg (Prop 1)]

-- equiv
test_equivFalsy = TestCase $ assertFalse $ equiv (Prop 1) (Prop 2)
test_equivFalsy2 = TestCase $ assertFalse $ equiv (Cnj props) (Dsj props) where props = [(Prop 1), (Prop 2)]
test_equivFalsy3 = TestCase $ assertFalse $ equiv (Impl f1 f2) (Equiv f1 f2) where f1 = Prop 1
                                                                                   f2 = Prop 2
prop_equivSame f = equiv f f


-- Exercise 3
-- isLiteral
test_isLiteralProp = TestCase $ assertTrue $ isLiteral (Prop 1)
test_isLiteralNegProp = TestCase $ assertTrue $ isLiteral (Neg(Prop 1))
test_isLiteralCnj = TestCase $ assertFalse $ isLiteral (Cnj [(Prop 1), (Prop 2)])
test_isLiteralDsj = TestCase $ assertFalse $ isLiteral (Dsj [(Prop 1), (Prop 2)])
test_isLiteralImpl = TestCase $ assertFalse $ isLiteral (Impl (Prop 1) (Prop 2))
test_isLiteralEquiv = TestCase $ assertFalse $ isLiteral (Equiv (Prop 1) (Prop 2))

-- isDisjunctionOfLiterals
test_isDisjunctionOfLiteralsProp = TestCase $ assertTrue $ isDisjunctionOfLiterals (Prop 1)
test_isDisjunctionOfLiteralsNegProp = TestCase $ assertTrue $ isDisjunctionOfLiterals (Neg(Prop 1))
test_isDisjunctionOfLiteralsDsjTruth = TestCase $ assertTrue $ isDisjunctionOfLiterals (Dsj [(Prop 1), Neg(Prop 2)])
test_isDisjunctionOfLiteralsDsjFalsy = TestCase $ assertFalse $ isDisjunctionOfLiterals (Dsj [(Prop 1), Impl (Prop 1) (Prop 2)])
test_isDisjunctionOfLiteralsCnj = TestCase $ assertFalse $ isDisjunctionOfLiterals (Cnj [(Prop 1), (Prop 2)])
test_isDisjunctionOfLiteralsImpl = TestCase $ assertFalse $ isDisjunctionOfLiterals (Impl (Prop 1) (Prop 2))
test_isDisjunctionOfLiteralsEquiv = TestCase $ assertFalse $ isDisjunctionOfLiterals (Equiv (Prop 1) (Prop 2))

-- isConjunctionOfDisjunctions
test_isConjunctionOfDisjunctionsProp = TestCase $ assertFalse $ isConjunctionOfDisjunctions (Prop 1)
test_isConjunctionOfDisjunctionsNegProp = TestCase $ assertFalse $ isConjunctionOfDisjunctions (Neg (Prop 1))
test_isConjunctionOfDisjunctionsCnjProps = TestCase $ assertTrue $ isConjunctionOfDisjunctions (Cnj [(Prop 1), Neg(Prop 2)])
test_isConjunctionOfDisjunctionsCnjDsjs = TestCase $ assertTrue $ isConjunctionOfDisjunctions (Cnj [(Dsj [(Prop 1), Neg(Prop 1)]), (Prop 1)])
test_isConjunctionOfDisjunctionsCnjDsjsNested = TestCase $ assertFalse $ isConjunctionOfDisjunctions (Cnj [(Dsj [Dsj [(Prop 1)]]), (Prop 1)])
test_isConjunctionOfDisjunctionsCnjCnjsNested = TestCase $ assertFalse $ isConjunctionOfDisjunctions (Cnj [(Cnj [(Prop 1)]), (Prop 1)])
test_isConjunctionOfDisjunctionsCnjDsjsMixed = TestCase $ assertFalse $ isConjunctionOfDisjunctions (Cnj [(Prop 1), (Impl (Prop 1) (Prop 2))])
test_isConjunctionOfDisjunctionsCnjDsjsMixed2 = TestCase $ assertFalse $ isConjunctionOfDisjunctions (Cnj [(Prop 1), (Equiv (Prop 1) (Prop 2))])
test_isConjunctionOfDisjunctionsDsj = TestCase $ assertFalse $ isConjunctionOfDisjunctions (Dsj [(Prop 1), (Prop 2)])
test_isConjunctionOfDisjunctionsEquiv = TestCase $ assertFalse $ isConjunctionOfDisjunctions (Equiv (Prop 1) (Prop 2))
test_isConjunctionOfDisjunctionsImpl = TestCase $ assertFalse $ isConjunctionOfDisjunctions (Impl (Prop 1) (Prop 2))

-- toCnf
prop_toCnf f = isInCnf cnfed && (equiv f cnfed) where cnfed = toCnf f

-- Exercise 4
formulaLength :: Form -> Int
formulaLength (Cnj f) = length f
formulaLength _ = 1

formulaPropsNumber :: Form -> Int
formulaPropsNumber (Prop _) = 1
formulaPropsNumber (Neg(Prop _)) = 1
formulaPropsNumber (Dsj f) = sum (map formulaPropsNumber f)
formulaPropsNumber (Cnj f) = sum (map formulaPropsNumber f)

formulaPropsMultiset :: Form -> [Int]
formulaPropsMultiset (Prop f) = [f]
formulaPropsMultiset (Neg(Prop f)) = [-f]
formulaPropsMultiset (Dsj f) = concatMap formulaPropsMultiset f
formulaPropsMultiset (Cnj f) = concatMap formulaPropsMultiset f

-- test the properties of the cnf2cls function
prop_cnf2cls :: Form -> Bool
prop_cnf2cls f = formulaLength cnfed == length c &&
                 formulaPropsNumber cnfed == sum (map length c) &&
                 sort (formulaPropsMultiset cnfed) == sort(concat c)
  where cnfed = toCnf f
        c = cnf2cls cnfed
