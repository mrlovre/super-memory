module LinguisticVariables.DistanceLinguisticVariables where

import           Domains.Domain
import           Domains.DomainElement
import           Domains.SimpleDomain
import           FuzzySets.CalculatedFuzzySet
import           FuzzySets.FuzzySet
import           FuzzySets.FuzzySetHelper

distance :: ADomain
distance = ADomain $ simpleDomain 0 1300

distanceDomainIndex :: Int -> Int
distanceDomainIndex e = indexOfElement distance $ domainElement [e]

criticalDistanceI :: Int
criticalDistanceI = distanceDomainIndex 30

closeDistanceI :: Int
closeDistanceI = distanceDomainIndex 50

farDistanceI :: Int
farDistanceI = distanceDomainIndex 100

distanceFuzzySet :: MembershipFunction -> AFuzzySet
distanceFuzzySet f = AFuzzySet $ calculatedFuzzySet f distance

criticallyCloseDistance :: AFuzzySet
criticallyCloseDistance = distanceFuzzySet $ lFunctionGenerator criticalDistanceI closeDistanceI

closeDistance :: AFuzzySet
closeDistance = distanceFuzzySet $ lambdaFunctionGenerator criticalDistanceI closeDistanceI farDistanceI

farDistance :: AFuzzySet
farDistance = distanceFuzzySet $ gammaFunctionGenerator closeDistanceI farDistanceI
