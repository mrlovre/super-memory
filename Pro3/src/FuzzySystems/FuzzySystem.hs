module FuzzySystems.FuzzySystem where

import           FuzzySystems.FuzzySystemHelper

class FuzzySystem a where
    conclude :: a -> Variables -> Double

data AFuzzySystem where
    AFuzzySystem :: (FuzzySystem a) => a -> AFuzzySystem

instance FuzzySystem AFuzzySystem where
    conclude (AFuzzySystem a) = conclude a
