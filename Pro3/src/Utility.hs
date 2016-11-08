module Utility where

readDoubles :: IO [Double]
readDoubles = map read . words <$> getLine
