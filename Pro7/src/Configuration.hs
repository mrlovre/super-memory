module Configuration where

nnConfiguration :: [Int]
nnConfiguration = [2, 6, 4, 3]

initMagnitude :: Double
initMagnitude = 10

mutationRate :: Double
mutationRate = 0.05

mutationTypeThreshold :: Double
mutationTypeThreshold = 0.9

additiveMutationMagnitude :: Double
additiveMutationMagnitude = 0.1

destructiveMutationMagnitude :: Double
destructiveMutationMagnitude = 10

populationSize :: Int
populationSize = 80

tournamentSelectionK :: Int
tournamentSelectionK = 5

nIterations :: Int
nIterations = 100000

maxError :: Double
maxError = 1e-7
