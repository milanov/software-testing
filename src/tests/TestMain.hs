{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} TestSol1
import {-@ HTF_TESTS @-} TestLab1BonusSolutions

main :: IO()
main = htfMain htf_importedTests
