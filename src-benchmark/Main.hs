import Criterion
import Criterion.Main

import Example (evalExample, examples)
import Naive (loop)

import Data.Vector

main :: IO ()
main = defaultMain $ benchExample <$> [0]

benchExample :: Int -> Benchmark
benchExample n = bench ("example " <> show n) (whnf (evalExample Naive.loop)  (examples ! n))