module LinguisticVariables.AngleLinguisticVariables where

import           Domains.Domain
import           Domains.DomainElement
import           Domains.SimpleDomain
import           FuzzySets.CalculatedFuzzySet
import           FuzzySets.FuzzySet
import           FuzzySets.FuzzySetHelper
import           LinguisticVariables.LinguisticVariableHelper

angle :: ADomain
angle = ADomain $ simpleDomain (-90) 90

angleDomainIndex :: Int -> Int
angleDomainIndex e = indexOfElement angle $ domainElement [e]

angleSharpRightI :: Int
angleSharpRightI = angleDomainIndex (-90)

angleRightI :: Int
angleRightI = angleDomainIndex (-45)

angleNeutralI :: Int
angleNeutralI = angleDomainIndex 0

angleLeftI :: Int
angleLeftI = angleDomainIndex 45

angleSharpLeftI :: Int
angleSharpLeftI = angleDomainIndex 90

angleFuzzySet :: MembershipFunction -> AFuzzySet
angleFuzzySet f = AFuzzySet $ calculatedFuzzySet f angle

sharpLeftAngle :: AFuzzySet
sharpLeftAngle = lvVery $ angleFuzzySet (gammaFunctionGenerator angleLeftI angleSharpLeftI)

sharpRightAngle :: AFuzzySet
sharpRightAngle = lvVery $ angleFuzzySet (lFunctionGenerator angleSharpRightI angleRightI)

leftAngle :: AFuzzySet
leftAngle = angleFuzzySet (lambdaFunctionGenerator angleNeutralI angleLeftI angleSharpLeftI)

rightAngle :: AFuzzySet
rightAngle = angleFuzzySet (lambdaFunctionGenerator angleSharpRightI angleRightI angleNeutralI)

neutralAngle :: AFuzzySet
neutralAngle = angleFuzzySet (lambdaFunctionGenerator angleRightI angleNeutralI angleLeftI)
