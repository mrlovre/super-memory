module Utility where

-- | Returns list of indices at diagonal of a d times d matrix.
diagonalIndices :: Int -> [Int]
diagonalIndices d = [0, d + 1 .. d * d]
