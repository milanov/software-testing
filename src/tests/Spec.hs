import Test.Hspec

import qualified Sol1Spec
import qualified Lab1BonusSpec
import qualified Lab2Spec
import qualified Lab3Spec

main :: IO ()
main = hspec $ do
  describe "Sol1"               Sol1Spec.spec
  describe "Lab1Bonus"          Lab1BonusSpec.spec
  describe "Lab2"               Lab2Spec.spec
  describe "Lab3"               Lab3Spec.spec