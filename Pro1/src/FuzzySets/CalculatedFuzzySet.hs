module FuzzySets.CalculatedFuzzySet where

import           Domains.Domain
import           FuzzySets.FuzzySet

import           Data.Vector        (Vector, (!))
import qualified Data.Vector        as V

data CalculatedFuzzySet where
    CalculatedFuzzySet :: ADomain -> Vector Double -> CalculatedFuzzySet

calculatedFuzzySet :: (Int -> Double) -> ADomain -> CalculatedFuzzySet
calculatedFuzzySet f d =
    CalculatedFuzzySet d (V.fromList $ map f [0 .. cardinality d - 1])

instance FuzzySet CalculatedFuzzySet where
    domain (CalculatedFuzzySet d _) = d
    valueAt (CalculatedFuzzySet d m) v = let
        index = indexOfElement d v
        in m ! index
