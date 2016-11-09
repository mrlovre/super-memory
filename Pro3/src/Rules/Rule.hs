module Rules.Rule where

import           Configuration
import           Domains.DomainElement
import           FuzzySets.CalculatedFuzzySet
import           FuzzySets.FuzzySet
import           FuzzySets.FuzzySetHelper
import           FuzzySystems.FuzzySystemHelper

type Rule = Variables -> AFuzzySet
type Ruleset = [Rule]

makeRule :: [(Int, AFuzzySet)] -> AFuzzySet -> AFuzzySet
makeRule antecedent consequent = let
    calcMembership e fs = valueAt fs (domainElement [e])
    memberships = map (uncurry calcMembership) antecedent
    total = foldl tNorm 1 memberships
    consequentDomain = domain consequent
    antecedentFuzzySet = constantFuzzySet total consequentDomain
    in AFuzzySet $ binaryOperation antecedentFuzzySet consequent sNorm
