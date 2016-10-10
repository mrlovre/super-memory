module Domain where

import           Dimensionable
import           DomainElement

-- | Defines a multi-component domain class.
class (Dimensionable a) => Domain a where
    -- | Cardinal number of domain.
    cardinality :: a -> Int
    -- | Returns n-th component of domain.
    getComponent :: a -> Int -> ADomain
    -- | Return the ordinal number of an element of the domain.
    indexOfElement :: a -> DomainElement -> Int
    -- | Retrieves n-th element from the domain.
    elementAtIndex :: a -> Int -> DomainElement
    -- | Returns an iterator through all elements of domain, in order.
    iterator :: a -> [DomainElement]
    -- | Returns list of all components.
    getAllComponents :: a -> [ADomain]
    getAllComponents domain = map (getComponent domain) [0 .. dimension domain - 1]
    -- | Returns first element of a domain.
    minElement :: a -> DomainElement
    minElement = head . iterator
    -- | Returns last element of a domain.
    maxElement :: a -> DomainElement
    maxElement = last . iterator

-- | An "any" domain wrapper around the 'Domain' class.
data ADomain where
     ADomain :: (Domain a) => a -> ADomain

instance Show ADomain where
    show (ADomain domain) = let
        fromZero = [0 ..] :: [Int]
        in unlines $ zipWith (++) (map ((++ "# element: ") . show) fromZero) (map show $ iterator domain)

instance Domain ADomain where
    cardinality (ADomain a) = cardinality a
    getComponent (ADomain a) = getComponent a
    indexOfElement (ADomain a) = indexOfElement a
    elementAtIndex (ADomain a) = elementAtIndex a
    iterator (ADomain a) = iterator a

instance Dimensionable ADomain where
    dimension (ADomain a) = dimension a
