-- | Fuzzy set, any-Fuzzy set.
module FuzzySets.FuzzySet where

import           Domains.Domain
import           Domains.DomainElement

-- | Type of a function which determines the grade of membership for an element of the fuzzy set.
type MembershipFunction = Int -> Double

-- | Model of a fuzzy set.
class FuzzySet a where
    -- | Retrieves domain of a fuzzy set.
    domain :: a -> ADomain
    -- | Gives grade of membership for a 'DomainElement' within the 'Domain'.
    valueAt :: a -> DomainElement -> Double

-- | An "any" fuzzy set wrapper around the 'FuzzySet' class.
data AFuzzySet where
    AFuzzySet :: (FuzzySet a) => a -> AFuzzySet

-- | "Any" fuzzy set instance.
instance FuzzySet AFuzzySet where
    domain (AFuzzySet a) = domain a
    valueAt (AFuzzySet a) = valueAt a

-- | Show instance.
instance Show AFuzzySet where
    show (AFuzzySet fuzzySet) = let
        d = domain fuzzySet
        display element = show element ++ " -> " ++ show (valueAt fuzzySet element)
        in unlines $ map display $ iterator d
