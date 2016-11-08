module Defuzzifiers.Defuzzifier where

class Defuzzifier a where

data ADefuzzifier where
    ADefuzzifier :: (Defuzzifier a) => a -> ADefuzzifier

instance Defuzzifier ADefuzzifier where
