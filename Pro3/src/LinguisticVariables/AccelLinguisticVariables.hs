module LinguisticVariables.AccelLinguisticVariables where

import           Domains.Domain
import           Domains.DomainElement
import           Domains.SimpleDomain
import           FuzzySets.CalculatedFuzzySet
import           FuzzySets.FuzzySet
import           FuzzySets.FuzzySetHelper

acceleration :: ADomain
acceleration = ADomain $ simpleDomain (-40) 40

accelerationDomainIndex :: Int -> Int
accelerationDomainIndex e = indexOfElement acceleration $ domainElement [e]

neutralAccelI :: Int
neutralAccelI = accelerationDomainIndex 0

topAccelI :: Int
topAccelI = accelerationDomainIndex 40

brakeI :: Int
brakeI = accelerationDomainIndex (-40)

normalAccelI :: Int
normalAccelI = accelerationDomainIndex 5

accelerationFuzzySet :: MembershipFunction -> AFuzzySet
accelerationFuzzySet f = AFuzzySet $ calculatedFuzzySet f acceleration

brake :: AFuzzySet
brake = accelerationFuzzySet $ lFunctionGenerator brakeI neutralAccelI

topAccel :: AFuzzySet
topAccel = accelerationFuzzySet $ gammaFunctionGenerator normalAccelI topAccelI

weakAccel :: AFuzzySet
weakAccel = accelerationFuzzySet $ lambdaFunctionGenerator neutralAccelI normalAccelI topAccelI

neutralAccel :: AFuzzySet
neutralAccel = accelerationFuzzySet $ lambdaFunctionGenerator (accelerationDomainIndex (-5)) neutralAccelI (accelerationDomainIndex 5)
