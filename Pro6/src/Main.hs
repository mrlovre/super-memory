module Main where

import           Control.Monad
import           System.Random.Shuffle

import           ANFIS
import           Sample


main :: IO ()
main = do
    anfis <- anfisInitialize 2 81
    let trainOnShuffledData a = do
            shuffledData <- shuffleM trainData
            let a' = trainANFIS shuffledData GradientDescent 0.01 a
                outputs = map (runANFIS a' . fst) trainData
            print $ (/ fromIntegral dataSize) $ sum $ map (** 2) $ zipWith (-) outputs expecteds
            return a'
        expecteds = map snd trainData
        dataSize  = length trainData
    void $ iterateWhileM (const True) trainOnShuffledData anfis

iterateWhileM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m [a]
iterateWhileM p r a = if p a
    then do
        a' <- r a
        (a :) <$> (iterateWhileM p r $! a')
    else return []
