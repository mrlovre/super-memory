module FuzzySystems.AccelFuzzySystemMin where

import FuzzySystems.FuzzySystem
import Defuzzifiers.Defuzzifier

data AccelFuzzySystemMin where

instance FuzzySystem AccelFuzzySystemMin where

accelFuzzySystemMin :: ADefuzzifier -> AccelFuzzySystemMin
accelFuzzySystemMin = undefined
