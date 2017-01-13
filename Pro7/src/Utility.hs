module Utility where

import           Control.Monad.Random
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as V
import           Statistics.Distribution
import           Statistics.Distribution.Normal

getGausses :: (MonadRandom mr) => Double -> mr [Double]
getGausses sigma = map (quantile $ normalDistr 0 sigma) <$> getRandoms

dot :: Vector Double -> Vector Double -> Double
v1 `dot` v2 = V.sum $ V.zipWith (*) v1 v2
