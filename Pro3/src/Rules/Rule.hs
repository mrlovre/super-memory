module Rules.Rule where

import           FuzzySets.FuzzySet

type Rule = AFuzzySet

rule :: (FuzzySet a) => a -> Rule
rule = AFuzzySet
