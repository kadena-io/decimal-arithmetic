module Main where


import Criterion
import Criterion.Main
import Numeric.Decimal

type PactDecimal = ExtendedDecimal P25

main :: IO ()
main =
  defaultMain [expBenches, lnBenches, sqrtBenches]

expBenches :: Benchmark
expBenches =
  bgroup "benchmarking EXP" [benchExp (fromIntegral n) | n <- [10, 100, 1000, 10000, 100000]]

lnBenches :: Benchmark
lnBenches =
  bgroup "benchmarking LN" [benchLn (fromIntegral n) | n <- [10, 100, 1000, 10000, 100000]]

sqrtBenches :: Benchmark
sqrtBenches =
  bgroup "benchmarking SQRT" [benchSqrt (fromIntegral n) | n <- [10, 100, 1000, 10000, 100000]]


benchExp :: PactDecimal -> Benchmark
benchExp n = do
  env (pure n) $ \ ~(n') ->
    let title = "benching exp(" <> show n <> ")"
    in bench title (nf exp n')

benchLn :: PactDecimal -> Benchmark
benchLn n = do
  env (pure n) $ \ ~(n') ->
    let title = "benching ln(" <> show n <> ")"
    in bench title (nf log n')

benchSqrt :: PactDecimal -> Benchmark
benchSqrt n = do
  env (pure n) $ \ ~(n') ->
    let title = "benching sqrt(" <> show n <> ")"
    in bench title (nf sqrt n')
