-- | Calculated fuzzy set.
module FuzzySets.CalculatedFuzzySet where

import           Domains.Domain
import           FuzzySets.FuzzySet

import           Data.Vector        (Vector, (!))
import qualified Data.Vector        as V

-- | 'CalculatedFuzzySet' is an instance of 'FuzzySet' where membership of each domain element is calculated using the
-- supplied function.
data CalculatedFuzzySet where
    CalculatedFuzzySet :: ADomain -> Vector Double -> CalculatedFuzzySet

-- | Constructor for 'CalculatedFuzzySet'. Takes a membership function and a 'Domain'.
calculatedFuzzySet :: (Int -> Double) -> ADomain -> CalculatedFuzzySet
calculatedFuzzySet f d =
    CalculatedFuzzySet d (V.fromList $ map f [0 .. cardinality d - 1])

-- | Fuzzy set instance.
instance FuzzySet CalculatedFuzzySet where
    domain (CalculatedFuzzySet d _) = d
    valueAt (CalculatedFuzzySet d m) v = let
        index = indexOfElement d v
        in m ! index
