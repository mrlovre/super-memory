module Main where

import           Data.Vector           ((!))
import qualified Data.Vector           as V
import           System.Environment
import           System.IO
import           System.Random.Shuffle

import           ANFIS
import           Layers
import           Sample
import           Utility

main :: IO ()
main = do
    args <- getArgs
    case args of
        [algorithm, m, learningRate, maxErr, maxIter] -> do
            anfisNet <- anfisInitialize 2 (read m)
            let trainOnShuffledData anfis = do
                    shuffledData <- shuffleM trainData
                    let anfis' = trainANFIS shuffledData algorithmV (read learningRate) anfis
                    hPrint stderr $ meanSquaredError $ outputs anfis'
                    return anfis'
                algorithmV = case algorithm of
                    "sgd" -> StochasticGradientDescent
                    "gd" -> GradientDescent
                    _ -> error "Invalid algorithm specified. Valid values are 'sgd' and 'gd'."
                outputs anfis = map (runANFIS anfis . fst) trainData
                meanSquaredError = (/ fromIntegral dataSize) . sum . map (** 2) . zipWith (-) expecteds
                expecteds = map snd trainData
                dataSize  = length trainData
            best <- (last . take (read maxIter)) <$> iterateUntilM ((< read maxErr) . meanSquaredError . outputs) trainOnShuffledData anfisNet
            print $ V.map mfa (membershipLayer best ! 0)
            print $ V.map mfb (membershipLayer best ! 0)
            print $ V.map mfa (membershipLayer best ! 1)
            print $ V.map mfb (membershipLayer best ! 1)
            print $ V.map tfp (transferLayer best)
            print $ V.map tfq (transferLayer best)
            print $ V.map tfr (transferLayer best)
        _ -> error "Usage: 'anfis algorithm m learningRate maxErr maxIter'"
