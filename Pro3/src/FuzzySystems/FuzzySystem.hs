module FuzzySystems.FuzzySystem where

class FuzzySystem a where
    conclude :: a -> [Double] -> Double

data AFuzzySystem where
    AFuzzySystem :: (FuzzySystem a) => a -> AFuzzySystem

instance FuzzySystem AFuzzySystem where
    conclude (AFuzzySystem a) = conclude a
