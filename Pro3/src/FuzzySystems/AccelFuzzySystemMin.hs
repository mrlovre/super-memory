module FuzzySystems.AccelFuzzySystemMin where

import           Defuzzifiers.Defuzzifier
import           FuzzySystems.FuzzySystem
import           Rules.Rule

data AccelFuzzySystemMin where
    AccelFuzzySystemMin :: [Rule] -> AccelFuzzySystemMin

instance FuzzySystem AccelFuzzySystemMin where

accelFuzzySystemMin :: ADefuzzifier -> AccelFuzzySystemMin
accelFuzzySystemMin = undefined
