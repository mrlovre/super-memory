module Rules.Rule where

import           FuzzySets.CalculatedFuzzySet
import           FuzzySets.FuzzySet

type Rule = AFuzzySet

rule :: (FuzzySet a) => a -> Rule
rule = AFuzzySet
