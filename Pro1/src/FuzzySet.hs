-- | 'FuzzySet', 'AFuzzySet'
module FuzzySet where

import           Domain
import           DomainElement

-- | Model of a fuzzy set.
class FuzzySet a where
    -- | Retrieves domain of a fuzzy set.
    domain :: a -> ADomain
    -- | Gives grade of membership for a 'DomainElement' within the 'Domain'.
    valueAt :: a -> DomainElement -> Double

-- | An "any" fuzzy set wrapper around the 'FuzzySet' class.
data AFuzzySet where
    AFuzzySet :: (FuzzySet a) => a -> AFuzzySet

instance FuzzySet AFuzzySet where
    domain (AFuzzySet a) = domain a
    valueAt (AFuzzySet a) = valueAt a

instance Show AFuzzySet where
    show (AFuzzySet fuzzySet) = let
        d = domain fuzzySet
        display element = show element ++ " -> " ++ show (valueAt fuzzySet element)
        in unlines $ map display $ iterator d
