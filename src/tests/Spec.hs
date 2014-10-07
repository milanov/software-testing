import Test.Hspec

import qualified Sol1Spec
import qualified Lab1BonusSpec
import qualified Lab2Spec
import qualified Lab3Spec
import qualified Lab4Spec
import qualified Lab5Spec

main :: IO ()
main = hspec $ do
  describe "Sol1"               Sol1Spec.spec
  describe "Lab1Bonus"          Lab1BonusSpec.spec
  describe "Lab2"               Lab2Spec.spec
  describe "Lab3"               Lab3Spec.spec
  describe "Lab4"               Lab4Spec.spec
  describe "Lab5"               Lab5Spec.spec