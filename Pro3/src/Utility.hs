module Utility where

readInts :: IO [Int]
readInts = map read . words <$> getLine

eps :: Double
eps = 1e-9
