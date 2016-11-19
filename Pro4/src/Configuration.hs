module Configuration where

populationSize :: Int
populationSize = 100

crossingMixThreshold :: Double
crossingMixThreshold = 0.25

chromosomeParameterBounds :: (Double, Double)
chromosomeParameterBounds = (-4, 4)

maxIter :: Int
maxIter = 1000

mutationRate :: Double
mutationRate = 0.1
