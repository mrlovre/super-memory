module GeneticAlgorithms where

import           Chromosome
import           Configuration
import           Evaluation
import           Utility

import           Control.Monad
import           Control.Monad.Random
import           Data.Vector          ((!), (//))
import qualified Data.Vector          as V


calculatePopulationScorePairs :: Dataset -> Population -> [(Double, Chromosome)]
calculatePopulationScorePairs dataset = map (\ c -> (evaluateDataset dataset c, c))

generationAlgorithm :: (MonadRandom m) => Bool -> Dataset -> Int -> Population -> m [(Double, Chromosome)]
generationAlgorithm ellitistic dataset nIter population = let
    go i currPop = let
        populationScorePairs = calculatePopulationScorePairs dataset currPop
        spawnChild = do
            [c1, c2, _] <- map snd . sortByScore <$> replicateM 3 (uniform populationScorePairs)
            child <- cross c1 c2
            mutate Configuration.mutationRate child
        in if
            | i == nIter -> return []
            | otherwise -> do
                let offspringSize = if ellitistic then Configuration.populationSize - 1 else Configuration.populationSize
                offspring <- replicateM offspringSize spawnChild
                let best = minimumByScore populationScorePairs
                    newPopulation = if ellitistic then snd best : offspring else offspring
                (best :) <$> go (succ nIter) newPopulation
    in go 0 population

eliminationAlgorithm :: (MonadRandom m) => Dataset -> Int -> Population -> m [(Double, Chromosome)]
eliminationAlgorithm dataset nIter population = let
    evaluateChromosome = evaluateDataset dataset
    go i currPopSc = let
        spawnChild = do
            indices <- replicateM 3 $ uniform [0 .. pred Configuration.populationSize]
            let [i1, i2, i3] = map snd $ sortByScore $ map (\ ind -> (fst $ currPopSc ! ind, ind)) indices
                [c1, c2] = map (snd . (currPopSc !)) [i1, i2]
            child <- cross c1 c2
            mutChild <- mutate Configuration.mutationRate child
            return (i3, (evaluateChromosome mutChild, mutChild))
        in if
            | i == nIter -> return []
            | otherwise -> do
                let best = minimumByScore $ V.toList currPopSc
                child <- spawnChild
                (best :) <$> go (succ nIter) (currPopSc // [child])
    in go 0 $ V.fromList $ calculatePopulationScorePairs dataset population
