module LinguisticVariables.SpeedLinguisticVariables where

import           Domains.Domain
import           Domains.DomainElement
import           Domains.SimpleDomain
import           FuzzySets.CalculatedFuzzySet
import           FuzzySets.FuzzySet
import           FuzzySets.FuzzySetHelper

speed :: ADomain
speed = ADomain $ simpleDomain 0 200

speedDomainIndex :: Int -> Int
speedDomainIndex e = indexOfElement speed $ domainElement [e]

zeroSpeedI :: Int
zeroSpeedI = speedDomainIndex 0

slowSpeedI :: Int
slowSpeedI = speedDomainIndex 20

moderateSpeedI :: Int
moderateSpeedI = speedDomainIndex 50

fastSpeedI :: Int
fastSpeedI = speedDomainIndex 100

topSpeedI :: Int
topSpeedI = speedDomainIndex 200

normalAccelI :: Int
normalAccelI = speedDomainIndex 20

speedFuzzySet :: MembershipFunction -> AFuzzySet
speedFuzzySet f = AFuzzySet $ calculatedFuzzySet f speed

idle :: AFuzzySet
idle = speedFuzzySet (lFunctionGenerator zeroSpeedI slowSpeedI)

slowSpeed :: AFuzzySet
slowSpeed = speedFuzzySet (lambdaFunctionGenerator zeroSpeedI slowSpeedI moderateSpeedI)

moderateSpeed :: AFuzzySet
moderateSpeed = speedFuzzySet (lambdaFunctionGenerator slowSpeedI moderateSpeedI fastSpeedI)

fastSpeed :: AFuzzySet
fastSpeed = speedFuzzySet (lambdaFunctionGenerator moderateSpeedI fastSpeedI topSpeedI)

topSpeed :: AFuzzySet
topSpeed = speedFuzzySet (gammaFunctionGenerator fastSpeedI topSpeedI)
