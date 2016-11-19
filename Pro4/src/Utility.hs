module Utility where

import           Control.Monad.Random
import           Data.List
import           Data.Ord

getChance :: (MonadRandom m) => m Double
getChance = getRandom

for :: [a] -> (a -> b) -> [b]
for = flip map

mean :: [Double] -> Double
mean l = let
    (totalN, totalS) = foldl (\ (n, s) d -> (n + 1, s + d)) (0, 0) l
    in totalS / totalN

sqDist :: Double -> Double -> Double
sqDist x1 x2 = (x1 - x2) ** 2

type Dataset = [((Double, Double), Double)]

type Score = Double

readDataset :: FilePath -> IO Dataset
readDataset path = do
    contents <- readFile path
    let parsed = map parseLine $ lines contents
        parseLine l = let
            [x, y, z] = map read $ words l :: [Double]
            in ((x, y), z)
    return parsed

sortByScore :: [(Score, b)] -> [(Score, b)]
sortByScore = sortBy (comparing fst)

nubByScore :: [(Score, b)] -> [(Score, b)]
nubByScore = nubBy (\ s1 s2 -> comparing fst s1 s2 == EQ)

minimumByScore :: [(Score, b)] -> (Score, b)
minimumByScore = minimumBy (comparing fst)

holdBest :: [(Score, b)] -> [(Int, (Score, b))]
holdBest [] = []
holdBest (l : ls) = let
    go _ [] = []
    go bestScore ((ind, m@(score, _)) : ms) = if
        | bestScore <= score -> go bestScore ms
        | otherwise -> (ind, m) : go score ms
    in (0, l) : go (fst l) (zip [1 ..] ls)
