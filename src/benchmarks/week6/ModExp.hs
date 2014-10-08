module ModExp (benchmarks) where

import Criterion (Benchmark, bench, nf)
import Week6
import Lab6

bigBase = 2988348162058574136915891421498819466320163312926952423791023078876139
bigExp = 2351399303373464486466122544523690094744975233415544072992656881240319
bigMod = 10 ^ 40

benchmarks :: [Benchmark]
benchmarks = 
    [ bench "a" (nf (exM' bigBase bigExp) bigMod),
      bench "a" (nf (expM bigBase bigExp) bigMod)
      bench "a" (nf (expMK bigBase bigExp) bigMod)]