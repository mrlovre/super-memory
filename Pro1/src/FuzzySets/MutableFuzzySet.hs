-- | Mutable fuzzy set.
module FuzzySets.MutableFuzzySet where

import           Control.Arrow
import           Data.Vector           (Vector, (!), (//))
import qualified Data.Vector           as V

import           Domains.Domain
import           Domains.DomainElement
import           FuzzySets.FuzzySet

-- | 'MutableFuzzySet' is an instance of 'FuzzySet' which is initially created as empty fuzzy set (i.e. membership of
-- all elements is zero). This instance of 'FuzzySet' supports update operations (using 'setMutableFuzzySet' and
-- 'updateMutableFuzzySet' functions), hence the name \'mutable\'.
data MutableFuzzySet where
    MutableFuzzySet :: ADomain -> Vector Double -> MutableFuzzySet

-- | Constructor for 'MutableFuzzySet'.
mutableFuzzySet :: ADomain -> MutableFuzzySet
mutableFuzzySet d = let
    card = cardinality d
    in MutableFuzzySet d (V.replicate card 0)

-- | Updates single domain element membership from the set.
setMutableFuzzySet :: MutableFuzzySet -> DomainElement -> Double -> MutableFuzzySet
setMutableFuzzySet (MutableFuzzySet d memberships) element value = let
    index = indexOfElement d element
    in MutableFuzzySet d (memberships // [(index, value)])

-- | Bulk update for the set. See 'setMutableFuzzySet' for updating single domain element.
updateMutableFuzzySet :: MutableFuzzySet -> [(DomainElement, Double)] -> MutableFuzzySet
updateMutableFuzzySet (MutableFuzzySet d memberships) elementValuePairs = let
    indexValuePairs = map (first (indexOfElement d)) elementValuePairs
    in MutableFuzzySet d (memberships // indexValuePairs)

-- | Fuzzy set instance.
instance FuzzySet MutableFuzzySet where
    domain (MutableFuzzySet d _) = d
    valueAt (MutableFuzzySet d m) i = let
        index = indexOfElement d i
        in m ! index
