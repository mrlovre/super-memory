{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE UndecidableInstances #-}
module Domain where

import           Control.Arrow
import           Control.Monad.State

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
    minElement :: a -> DomainElement
    minElement = head . iterator
    maxElement :: a -> DomainElement
    maxElement = last . iterator

-- | An "any" domain wrapper around the Domain class.
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

-- | Defines a class with a dimension property.
class Dimensionable a where
    -- | Returns the dimension of a domain element.
    dimension :: a -> Int

-- | Model of a multi-dimension domain element.
newtype DomainElement = DomainElement [Int] deriving (Eq)

instance Show DomainElement where
    show (DomainElement a) = show a

extractDomainElement :: DomainElement -> [Int]
extractDomainElement (DomainElement e) = e

createDomainElement :: [Int] -> DomainElement
createDomainElement [] = error "Domain element must contain at least one component."
createDomainElement a  = DomainElement a

instance Dimensionable DomainElement where
    dimension (DomainElement a) = length a

getComponentValue :: Int -> DomainElement -> Int
getComponentValue n (DomainElement a) = a !! n

data SimpleDomain = SimpleDomain Int Int

createSimpleDomain :: Int -> Int -> SimpleDomain
createSimpleDomain start end = if
    | start > end -> error "Domain start must be less than or equal to the domain end."
    | otherwise -> SimpleDomain start end

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

    elementAtIndex domain@(SimpleDomain start _) n = if
        | n < 0 || n >= cardinality domain -> error "Index exceeds domain boundaries."
        | otherwise -> DomainElement [start + n]

    iterator (SimpleDomain start end) = map (DomainElement . (:[])) [start .. end]

    minElement (SimpleDomain start _) = DomainElement [start]

    maxElement (SimpleDomain _ end) = DomainElement [end]

instance Dimensionable SimpleDomain where
    dimension _ = 1

data CompositeDomain where
    CompositeDomain :: [SimpleDomain] -> CompositeDomain

createCompositeDomain :: [SimpleDomain] -> CompositeDomain
createCompositeDomain [] = error "Composite domain must contain at least one component."
createCompositeDomain other = CompositeDomain other

instance Domain CompositeDomain where
    cardinality (CompositeDomain a) = product $ map cardinality a

    getComponent domain@(CompositeDomain a) n = if
        | n < 0 || n >= dimension domain -> error "Component number out of range."
        | otherwise -> ADomain $ a !! n

    indexOfElement domain@(CompositeDomain a) domainElement@(DomainElement b) = let
        cardinalities = map cardinality a
        startElements = map (\ (SimpleDomain start _) -> start) a
        normalizedIndices = zipWith (-) b startElements
        domainElementDimension = dimension domainElement
        lastDimension = getComponentValue (domainElementDimension - 1) domainElement
        in if
            | domainElementDimension /= dimension domain ->
                error "Domain element is not of equal dimension as the domain."
            | or $ zipWith (\ x y -> x < 0 || x >= y) normalizedIndices cardinalities ->
                error "Domain element is not contained in the domain."
            | otherwise -> sum (zipWith (*) normalizedIndices (tail cardinalities)) + lastDimension

    elementAtIndex domain@(CompositeDomain a) n = let
        cardinalities = map cardinality a
        startElements = map (\ (SimpleDomain start _) -> start) a
        elementIndices = fst $ flip execState ([], n) $
            mapM (\ c -> modify (\ (s, m) -> (s ++ [m `div` c], m `mod` c))) $ tail cardinalities ++ [1]
        in if
            | n < 0 || n >= cardinality domain -> error "Index exceeds domain boundaries."
            | otherwise -> DomainElement $ zipWith (+) elementIndices startElements

    iterator (CompositeDomain a) = let
        subdomainElements = map (map extractDomainElement . iterator) a
        in map DomainElement $ foldr cartesianProduct [[]] subdomainElements

-- | Creates a cartesian product of two sets, i.e. pairs each element of the first set with an element of the second
-- set, in order.
-- Equal to (without list comprehension): \ a b -> concatMap (\ i -> map (\ j -> i ++ j) b) a
cartesianProduct :: [[a]] -> [[a]] -> [[a]]
cartesianProduct a b = [i ++ j | i <- a, j <- b]

instance Dimensionable CompositeDomain where
    dimension (CompositeDomain a) = length a

intRange :: Int -> Int -> SimpleDomain
intRange a b = createSimpleDomain a (b - 1)

combine :: (Domain a) => [a] -> CompositeDomain
combine domains = let
    components = concatMap getAllComponents domains
    minMax = map (join (***) (head . extractDomainElement) . (minElement &&& maxElement)) components
    simpleSubdomains = map (uncurry SimpleDomain) minMax
    in CompositeDomain simpleSubdomains
