module Instances.CompositeDomainInstances where

import           Dimensionable
import           Domain
import           DomainElement
import           Domains.CompositeDomain
import           Domains.SimpleDomain
import           Instances.SimpleDomainInstances ()
import           Utility

import           Control.Monad.State

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
