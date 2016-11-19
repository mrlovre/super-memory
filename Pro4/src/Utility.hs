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

readDataset :: FilePath -> IO Dataset
readDataset path = do
    contents <- readFile path
    let parsed = map parseLine $ lines contents
        parseLine l = let
            [x, y, z] = map read $ words l :: [Double]
            in ((x, y), z)
    return parsed

sortByScore :: [(Double, b)] -> [(Double, b)]
sortByScore = sortBy (comparing fst)
