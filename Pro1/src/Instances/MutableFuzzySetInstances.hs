module Instances.MutableFuzzySetInstances where

import           Domain
import           FuzzySet
import           FuzzySets.MutableFuzzySet

import           Data.Vector               ((!))

instance FuzzySet MutableFuzzySet where
    domain (MutableFuzzySet d _) = d
    valueAt (MutableFuzzySet d m) i = let
        index = indexOfElement d i
        in m ! index
