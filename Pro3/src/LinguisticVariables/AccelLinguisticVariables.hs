module LinguisticVariables.AccelLinguisticVariables where

import           Domains.Domain
import           Domains.DomainElement
import           Domains.SimpleDomain
import           FuzzySets.CalculatedFuzzySet
import           FuzzySets.FuzzySet
import           FuzzySets.FuzzySetHelper

acceleration :: ADomain
acceleration = ADomain $ simpleDomain (-200) 200

accelerationDomainIndex :: Int -> Int
accelerationDomainIndex e = indexOfElement acceleration $ domainElement [e]

neutralAccelI :: Int
neutralAccelI = accelerationDomainIndex 0

topAccelI :: Int
topAccelI = accelerationDomainIndex 100

brakeI :: Int
brakeI = accelerationDomainIndex (-100)

normalAccelI :: Int
normalAccelI = accelerationDomainIndex 20

accelerationFuzzySet :: MembershipFunction -> AFuzzySet
accelerationFuzzySet f = AFuzzySet $ calculatedFuzzySet f acceleration

topAccel :: AFuzzySet
topAccel = accelerationFuzzySet $ gammaFunctionGenerator normalAccelI topAccelI

weakAccel :: AFuzzySet
weakAccel = accelerationFuzzySet $ lambdaFunctionGenerator neutralAccelI normalAccelI topAccelI
