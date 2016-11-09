module LinguisticVariables.DistanceLinguisticVariables where

import           Domains.Domain
import           Domains.DomainElement
import           FuzzySets.CalculatedFuzzySet
import           FuzzySets.FuzzySet
import           FuzzySets.FuzzySetHelper

distance :: ADomain
distance = undefined

zeroDistanceI :: Int
zeroDistanceI = indexOfElement distance $ domainElement [0]

criticalDistanceI :: Int
criticalDistanceI = indexOfElement distance $ domainElement [25]

farDistanceI :: Int
farDistanceI = indexOfElement distance $ domainElement [80]

farthestDistanceI :: Int
farthestDistanceI = indexOfElement distance $ domainElement [100]

criticallyCloseDistance :: AFuzzySet
criticallyCloseDistance = AFuzzySet $ calculatedFuzzySet (lFunctionGenerator zeroDistanceI criticalDistanceI) distance

farDistance :: AFuzzySet
farDistance = AFuzzySet $ calculatedFuzzySet (gammaFunctionGenerator farDistanceI farthestDistanceI) distance
