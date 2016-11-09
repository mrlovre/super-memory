module FuzzySystems.FuzzySystem where

import           Configuration
import           Defuzzifiers.Defuzzifier
import           Domains.Domain
import           FuzzySets.FuzzySetHelper
import           FuzzySystems.FuzzySystemHelper
import           Rules.Rule

class FuzzySystem a where
    conclude :: a -> Variables -> Int
    conclude fs vars = let
        rs = getRuleset fs
        d = getDomain fs
        def = getDefuzzifier fs
        conclusions = map ($ vars) rs
        total = foldl (\ fs1 fs2 -> binaryOperation fs1 fs2 sNorm) (emptyFuzzySet d) conclusions
        in defuzzify def total
    getRuleset :: a -> Ruleset
    getDomain :: a -> ADomain
    getDefuzzifier :: a -> ADefuzzifier

data AFuzzySystem where
    AFuzzySystem :: (FuzzySystem a) => a -> AFuzzySystem

instance FuzzySystem AFuzzySystem where
    getRuleset (AFuzzySystem a) = getRuleset a
    getDomain (AFuzzySystem a) = getDomain a
    getDefuzzifier (AFuzzySystem a) = getDefuzzifier a
