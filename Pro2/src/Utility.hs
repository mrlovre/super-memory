module Utility where

-- | Returns list of indices at diagonal of a d times d matrix.
diagonalIndices :: Int -> [Int]
diagonalIndices d = [0, d + 1 .. d * d]

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1 ..]

takeWhileIncluding :: (a -> Bool) -> [a] -> [a]
takeWhileIncluding _ [] = []
takeWhileIncluding p (l:rest) = if
    | p l -> l : takeWhileIncluding p rest
    | otherwise -> [l]
