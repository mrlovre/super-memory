module Utility where

import           Control.Monad.Random
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as V
import           Statistics.Distribution
import           Statistics.Distribution.Normal

getGausses :: (MonadRandom mr) => Double -> mr [Double]
getGausses sigma = map (quantile $ normalDistr 0 sigma) <$> getRandoms

getGauss :: (MonadRandom mr) => Double -> mr Double
getGauss sigma = quantile (normalDistr 0 sigma) <$> getRandom

dot :: Vector Double -> Vector Double -> Double
v1 `dot` v2 = V.sum $ V.zipWith (*) v1 v2

scalarDot :: Double -> Vector Double -> Vector Double
c `scalarDot` v = V.map (c *) v

scalarDiv :: Vector Double -> Double -> Vector Double
v `scalarDiv` c = V.map (/ c) v

addV :: Vector Double -> Vector Double -> Vector Double
addV = V.zipWith (+)

subV :: Vector Double -> Vector Double -> Vector Double
subV = V.zipWith (-)
