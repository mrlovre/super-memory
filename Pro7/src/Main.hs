module Main where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Random
import           Data.List
import           Data.Ord
import           Data.Vector           (Vector, (!))
import qualified Data.Vector           as V
import           System.Random.Shuffle

import qualified Configuration
import           Genetic
import           NN
import           Utility

main :: IO ()
main = do
    dataset <- map (map read . words) . lines <$> readFile "dataset.txt" :: IO [[Double]]
    let (samples, labels) = join (***) V.fromList $ (map fst &&& map snd) $ divideClasses dataset
        datasetSize = V.length samples
    population <- V.replicateM Configuration.populationSize $ nn Configuration.nnConfiguration
    let iteration currPop = do
            let populationErrors = V.map (\ p -> (p, mse p)) currPop
                (currentBest, currentScore) = minimumBy (comparing snd) populationErrors
                produceOffspring = do
                    randomK <- take Configuration.tournamentSelectionK <$> shuffleM (V.toList populationErrors)
                    let [parent1, parent2] = take 2 $ map fst $ sortBy (comparing snd) randomK
                    pCross <- getRandom :: IO Double
                    child <- if
                        | pCross < 1.0 / 3 -> cross1 parent1 parent2
                        | pCross < 2.0 / 3 -> cross2 parent1 parent2
                        | otherwise        -> cross3 parent1 parent2
                    pMutate <- getRandom :: IO Double
                    if pMutate < Configuration.mutationTypeThreshold
                        then mutate1 child
                        else mutate2 child
            offspring <- V.replicateM (pred Configuration.populationSize) produceOffspring
            return $ currentBest `V.cons` offspring
        mse p = let
            outputs = V.map (nnForwardPass p) samples
            in squareDistance outputs labels / fromIntegral datasetSize
        train = let
            doTrain i currPop = do
                newPop <- iteration currPop
                let best = V.head newPop
                    score = mse best
                putStrLn $ "Iteration #" ++ show i ++ ", score: " ++ show score
                if score < Configuration.maxError || i == Configuration.nIterations
                    then return best
                    else doTrain (succ i) newPop
            in doTrain 0
    solution <- train population
    print solution
