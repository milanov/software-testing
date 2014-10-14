module ModExp (benchmarks) where

import Criterion (Benchmark, bench, nf)
import Week6
import Lab6

bigBase = 2988348162
bigExp = 13993024
bigMod = 10 ^ 20

benchmarks :: [Benchmark]
benchmarks = 
    [ bench "expM - native implementation" (nf (expM bigBase bigExp) bigMod),
      bench "exMReal - our implementation" (nf (exMReal bigBase bigExp) bigMod),
      bench "exMWiki - wiki suggested implementation" (nf (exMWiki bigBase bigExp) bigMod)]