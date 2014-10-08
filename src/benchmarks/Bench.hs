module Main (main) where

import Criterion.Main(bgroup, defaultMain)
import qualified ModExp


main :: IO ()
main = defaultMain
    [ bgroup "ModExp" ModExp.benchmarks
    ]