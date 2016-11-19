module Main where

import           Chromosome
import qualified Configuration
import           Evaluation
import           Utility

import           Control.Monad
import           Control.Monad.Random

dataPath :: FilePath
dataPath = "/home/frka/haskell/NENR/Pro4/datasets/dataset1"

main :: IO ()
main = do
    population <- evalRandIO $ replicateM Configuration.populationSize randomChromosome
    dataset <- readDataset dataPath
    bests <- evalRandIO $ geneticAlgorithm dataset Configuration.maxIter population
    putStrLn $ unlines $ map show bests

calculatePopulationScorePairs :: Dataset -> Population -> [(Double, Chromosome)]
calculatePopulationScorePairs dataset = map (\ c -> (evaluateDataset dataset c, c))

geneticAlgorithm :: (MonadRandom m) => Dataset -> Int -> Population -> m [(Double, Chromosome)]
geneticAlgorithm dataset nIter currPop = let
    populationScorePairs = calculatePopulationScorePairs dataset currPop
    sorted = sortByScore populationScorePairs
    spawnChild = do
        [c1, c2, _] <- map snd . sortByScore <$> replicateM 3 (uniform populationScorePairs)
        child <- cross c1 c2
        mutate Configuration.mutationRate child
    in if
        | nIter == 0 -> return []
        | otherwise -> do
            newPopulation <- replicateM Configuration.populationSize spawnChild
            let best = head sorted
            (best :) <$> geneticAlgorithm dataset (pred nIter) newPopulation
