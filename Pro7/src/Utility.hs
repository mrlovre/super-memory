module Utility where

import           Control.Monad.Random
import           Data.Function
import           Data.List
import           Data.Ord
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

divideClasses :: [[Double]] -> [(Vector Double, Vector Double)]
divideClasses dataset = let
    go (x : y : oneHot) = (V.fromList [x, y], V.fromList oneHot)
    in map go dataset

squareDistance :: Vector (Vector Double) -> Vector (Vector Double) -> Double
squareDistance v1 v2 = V.sum $ V.map (V.sum . V.map (** 2)) $ V.zipWith subV v1 v2

sigmoid :: Double -> Double
sigmoid = (1/) . (1 +) . exp . negate

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p r a = if
    | p a -> return a
    | otherwise -> do
        a' <- r a
        untilM p r $! a'
