module Configuration where

populationSize :: Int
populationSize = 100

crossingMixThreshold :: Double
crossingMixThreshold = 0.4

chromosomeParameterBounds :: (Double, Double)
chromosomeParameterBounds = (-4, 4)

maxIter :: Int
maxIter = 100000

mutationRate :: Double
mutationRate = 0.1

mutationMagnitude :: Double
mutationMagnitude = 0.1
