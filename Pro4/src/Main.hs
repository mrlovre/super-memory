module Main where

import           Chromosome
import qualified Configuration
import           GeneticAlgorithms
import           Utility

import           Control.Monad
import           Control.Monad.Random

datasetPath :: FilePath
datasetPath = "/home/frka/haskell/NENR/Pro4/datasets/dataset1"

main :: IO ()
main = do
    population <- evalRandIO $ replicateM Configuration.populationSize randomChromosome
    dataset <- readDataset datasetPath
    bests <- evalRandIO $ eliminationAlgorithm dataset Configuration.maxIter population
    putStrLn $ unlines $ map prettyPrint $ holdBest bests

prettyPrint :: (Int, (Score, Chromosome)) -> String
prettyPrint (i, (s, c)) = unlines ["Iteration #" ++ show i, "Score: " ++ show s, show c]
