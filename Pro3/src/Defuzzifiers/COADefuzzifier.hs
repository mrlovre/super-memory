module Defuzzifiers.COADefuzzifier where

import           Defuzzifiers.Defuzzifier
import           Domains.Domain
import           Domains.DomainElement
import           FuzzySets.FuzzySet
import           Utility

data COADefuzzifier where
    COADefuzzifier :: COADefuzzifier

instance Defuzzifier COADefuzzifier where
    defuzzify _ fs = let
        dom = domain fs
        it = iterator dom
        (sumOfMemberships, sumOfValues) =
            foldl (\ (a, b) v -> (a + valueAt fs v, b + fromIntegral (head (extractDomainElement v)) * valueAt fs v)) (0, 0) it
        in if
            | abs sumOfValues < eps && abs sumOfMemberships < eps -> 0
            | otherwise -> round $ sumOfValues / sumOfMemberships

coaDefuzzifier :: COADefuzzifier
coaDefuzzifier = COADefuzzifier
