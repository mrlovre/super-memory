module Utility where

readInts :: IO [Int]
readInts = map read . words <$> getLine
