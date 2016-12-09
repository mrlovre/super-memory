module Chromosome where

import qualified Configuration
import           Utility

import           Control.Monad
import           Control.Monad.Random
import           Data.List

data Chromosome where
    Chromosome :: { b0 :: Double, b1 :: Double, b2 :: Double, b3 :: Double, b4 :: Double } -> Chromosome

instance Show Chromosome where
    show Chromosome{ .. } = "Chromosome:\n  { "
        ++ intercalate ",  " (zipWith (++) ["b0 = ", "b1 = ", "b2 = ", "b3 = ", "b4 = "] (map show [b0, b1, b2, b3, b4]))
        ++ " }"

deriving instance Eq Chromosome

type Population = [Chromosome]

randomChromosome :: (MonadRandom m) => m Chromosome
randomChromosome = do
    let (lowerBound, upperBound) = Configuration.chromosomeParameterBounds
    [b0, b1, b2, b3, b4] <- replicateM 5 ((+ lowerBound) . (* (upperBound - lowerBound)) <$> getChance)
    return Chromosome{ .. }

extractParameters :: Chromosome -> [Double]
extractParameters Chromosome{ .. } = [b0, b1, b2, b3, b4]

mutate :: (MonadRandom m) => Double -> Chromosome -> m Chromosome
mutate p c = do
    chances <- replicateM 5 getChance
    let params = extractParameters c
    [b0, b1, b2, b3, b4] <- forM (zip chances params) $
        \ (chance, param) -> if
            | chance < p -> do
                mutation <- (* Configuration.mutationMagnitude) . (+ negate 0.5) <$> getChance
                return $ param + mutation
            | otherwise -> return param
    return Chromosome{ .. }

cross :: (MonadRandom m) => Chromosome -> Chromosome -> m Chromosome
cross c1 c2 = do
    chances <- replicateM 5 getChance
    let mixThr = Configuration.crossingMixThreshold
        [paramsC1, paramsC2] = map extractParameters [c1, c2]
        [b0, b1, b2, b3, b4] = for (zip3 chances paramsC1 paramsC2) $
            \ (chance, paramC1, paramC2) -> if
                | chance < mixThr -> paramC1
                | chance < 1 - mixThr -> paramC1 + (1.1 * (chance - mixThr) - 0.55 * (0.5 - mixThr)) * (paramC2 - paramC1) / (1 - 2 * mixThr)
                | otherwise -> paramC2
    return Chromosome{ .. }

evaluate :: Chromosome -> (Double, Double) -> Double
evaluate Chromosome{ .. } (x, y) = let
    sn = sin (b0 + b1 * x)
    cs = b2 * cos (x * (b3 + y))
    ex = 1 / (1 + exp ((x - b4) ** 2))
    in sn + cs * ex
