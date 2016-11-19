module LinguisticVariables.AccelLinguisticVariables where

import           Domains.Domain
import           Domains.DomainElement
import           Domains.SimpleDomain
import           FuzzySets.CalculatedFuzzySet
import           FuzzySets.FuzzySet
import           FuzzySets.FuzzySetHelper

acceleration :: ADomain
acceleration = ADomain $ simpleDomain (-80) 80

accelerationDomainIndex :: Int -> Int
accelerationDomainIndex e = indexOfElement acceleration $ domainElement [e]

neutralAccelI :: Int
neutralAccelI = accelerationDomainIndex 0

topAccelI :: Int
topAccelI = accelerationDomainIndex 80

brakeI :: Int
brakeI = accelerationDomainIndex (-80)

normalAccelI :: Int
normalAccelI = accelerationDomainIndex 32

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

slightDeccel :: AFuzzySet
slightDeccel = accelerationFuzzySet $ lambdaFunctionGenerator (accelerationDomainIndex (-10)) (accelerationDomainIndex (-5)) (accelerationDomainIndex 0)
