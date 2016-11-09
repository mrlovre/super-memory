module Defuzzifiers.Defuzzifier where

import           FuzzySets.FuzzySet

class Defuzzifier a where
    defuzzify :: a -> AFuzzySet -> Int

data ADefuzzifier where
    ADefuzzifier :: (Defuzzifier a) => a -> ADefuzzifier

instance Defuzzifier ADefuzzifier where
    defuzzify (ADefuzzifier a) = defuzzify a
