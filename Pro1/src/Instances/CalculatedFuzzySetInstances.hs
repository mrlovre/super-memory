module Instances.CalculatedFuzzySetInstances where

import           Domain
import           FuzzySet
import           FuzzySets.CalculatedFuzzySet

import           Data.Vector                  ((!))

instance FuzzySet CalculatedFuzzySet where
    domain (CalculatedFuzzySet d _) = d
    valueAt (CalculatedFuzzySet d m) v = let
        index = indexOfElement d v
        in m ! index
