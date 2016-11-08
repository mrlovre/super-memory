module FuzzySystems.RudderFuzzySystemMin where

import FuzzySystems.FuzzySystem
import Defuzzifiers.Defuzzifier

data RudderFuzzySystemMin where

instance FuzzySystem RudderFuzzySystemMin where

rudderFuzzySystemMin :: ADefuzzifier -> RudderFuzzySystemMin
rudderFuzzySystemMin = undefined
