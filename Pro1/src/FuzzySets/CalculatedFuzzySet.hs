module FuzzySets.CalculatedFuzzySet where

import           Domain

import           Data.Vector (Vector)
import qualified Data.Vector as V

data CalculatedFuzzySet where
    CalculatedFuzzySet :: ADomain -> Vector Double -> CalculatedFuzzySet

calculatedFuzzySet function domain =
    CalculatedFuzzySet domain (V.fromList $ map function [0 .. cardinality domain - 1])
