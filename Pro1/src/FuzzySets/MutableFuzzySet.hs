module FuzzySets.MutableFuzzySet where

import           Control.Arrow
import           Data.Vector           (Vector, (!), (//))
import qualified Data.Vector           as V

import           Domains.Domain
import           Domains.DomainElement
import           FuzzySets.FuzzySet

data MutableFuzzySet where
    MutableFuzzySet :: ADomain -> Vector Double -> MutableFuzzySet

mutableFuzzySet :: ADomain -> MutableFuzzySet
mutableFuzzySet d = let
    card = cardinality d
    in MutableFuzzySet d (V.replicate card 0)

setMutableFuzzySet :: MutableFuzzySet -> DomainElement -> Double -> MutableFuzzySet
setMutableFuzzySet (MutableFuzzySet d memberships) element value = let
    index = indexOfElement d element
    in MutableFuzzySet d (memberships // [(index, value)])

updateMutableFuzzySet :: MutableFuzzySet -> [(DomainElement, Double)] -> MutableFuzzySet
updateMutableFuzzySet (MutableFuzzySet d memberships) elementValuePairs = let
    indexValuePairs = map (first (indexOfElement d)) elementValuePairs
    in MutableFuzzySet d (memberships // indexValuePairs)

instance FuzzySet MutableFuzzySet where
    domain (MutableFuzzySet d _) = d
    valueAt (MutableFuzzySet d m) i = let
        index = indexOfElement d i
        in m ! index
