{-# LANGUAGE GADTs      #-}
{-# LANGUAGE MultiWayIf #-}
module Domain where

-- | Defines a multi-component domain class.
class Domain a where
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

data ADomain where
    ADomain :: Domain a => a -> ADomain

-- | Defines a class with a dimension property.
class Dimensionable a where
    -- | Returns the dimension of a domain element.
    dimension :: a -> Int

-- | Model of a multi-dimension domain element.
newtype DomainElement = DomainElement [Int] deriving Eq

instance Dimensionable DomainElement where
    dimension (DomainElement a) = length a

getComponentValue :: Int -> DomainElement -> Int
getComponentValue n (DomainElement a) = a !! n

data SimpleDomain = SimpleDomain Int Int

instance Domain SimpleDomain where
    cardinality (SimpleDomain start end) = end - start + 1
    getComponent domain 0 = ADomain domain
    getComponent _ _      = error "Simple domain contains only one component."
    indexOfElement domain@(SimpleDomain start end) domainElement = let
        e = getComponentValue 0 domainElement
        in if
            | dimension domainElement /= dimension domain ->
                error "Domain element is not of equal dimension as the domain."
            | e < start || e > end -> error "Domain element is not contained in the domain."
            | otherwise -> e - start
    elementAtIndex domain@(SimpleDomain start _) n
        | n < 0 || n > cardinality domain = error "Index exceeds domain boundaries."
        | otherwise = DomainElement [start + n]
    iterator (SimpleDomain start end) = map (DomainElement . (:[])) [start .. end]

instance Dimensionable SimpleDomain where
    dimension _ = 1

data CompositeDomain where
    CompositeDomain :: [SimpleDomain] -> CompositeDomain

instance Domain CompositeDomain where
    cardinality (CompositeDomain a) = product $ map cardinality a
    getComponent domain@(CompositeDomain a) n = if
        | n < 0 || n >= dimension domain -> error "Component number out of range."
        | otherwise -> ADomain $ a !! n
    indexOfElement domain@(CompositeDomain a) domainElement@(DomainElement b) = let
        cardinalities = map cardinality a
        startElements = map (\(SimpleDomain start _) -> start) a
        normalizedIndices = zipWith (-) b startElements
        domainElementDimension = dimension domainElement
        lastDimension = getComponentValue (domainElementDimension - 1) domainElement
        in if
            | domainElementDimension /= dimension domain ->
                error "Domain element is not of equal dimension as the domain."
            | or $ zipWith (\x y -> x < 0 || x >= y) normalizedIndices cardinalities ->
                error "Domain element is not contained in the domain."
            | otherwise -> sum (zipWith (*) normalizedIndices (tail cardinalities)) + lastDimension
    elementAtIndex domain@(CompositeDomain a) n = let
        cardinalities = map cardinality a
        startElements = map (\(SimpleDomain start _) -> start) a

        in if
        | n < 0 || n > cardinality domain -> error "Index exceeds domain boundaries."



instance Dimensionable CompositeDomain where
    dimension (CompositeDomain a) = length a
