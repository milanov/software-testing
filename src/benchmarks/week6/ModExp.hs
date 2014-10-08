module ModExp (benchmarks) where

import Criterion (Benchmark, bench, nf)
import Week6
import Lab6

benchmarks :: [Benchmark]
benchmarks = 
    [ bench "a" (nf (exM' 2 17) 5),
      bench "a" (nf (expM 2 17) 5)]