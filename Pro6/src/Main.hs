module Main where

import           ANFIS
import           Sample


main :: IO ()
main = do
    anfis <- anfisInitialize 2 20
    let trained   = iterate (trainANFIS trainData StochasticGradientDescent 0.001) anfis !! 1000
        outputs'  = map (runANFIS anfis . fst) trainData
        outputs   = map  (runANFIS trained . fst) trainData
        expecteds = map snd trainData
    print $ zipWith (-) outputs' expecteds
    print $ zipWith (-) outputs expecteds
