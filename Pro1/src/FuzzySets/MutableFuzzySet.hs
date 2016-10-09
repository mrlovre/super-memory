module FuzzySets.MutableFuzzySet where

import           Control.Arrow
import           Data.Vector   (Vector, (//))
import qualified Data.Vector   as V

import           Domain
import           DomainElement

data MutableFuzzySet where
    MutableFuzzySet :: ADomain -> Vector Double -> MutableFuzzySet

mutableFuzzySet domain = let
    card = cardinality domain
    in MutableFuzzySet domain (V.replicate card 0)

setMutableFuzzySet :: MutableFuzzySet -> DomainElement -> Double -> MutableFuzzySet
setMutableFuzzySet (MutableFuzzySet domain memberships) element value = let
    index = indexOfElement domain element
    in MutableFuzzySet domain (memberships // [(index, value)])

updateMutableFuzzySet :: MutableFuzzySet -> [(DomainElement, Double)] -> MutableFuzzySet
updateMutableFuzzySet (MutableFuzzySet domain memberships) elementValuePairs = let
    indexValuePairs = map (first (indexOfElement domain)) elementValuePairs
    in MutableFuzzySet domain (memberships // indexValuePairs)
