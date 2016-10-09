module DomainHelper where

import           Domain
import           DomainElement
import           Domains.CompositeDomain
import           Domains.SimpleDomain

import           Control.Arrow
import           Control.Monad

-- | Creates a 'SimpleDomain' containing range from a (inclusive) to b (exclusive).
intRange :: Int -> Int -> SimpleDomain
intRange a b = createSimpleDomain a (b - 1)

-- | Creates a 'CompositeDomain' as a CartesianProduct of supplied domains.
combine :: (Domain a) => [a] -> CompositeDomain
combine domains = let
    components = concatMap getAllComponents domains
    minMax = map (join (***) (head . extractDomainElement) . (minElement &&& maxElement)) components
    simpleSubdomains = map (uncurry SimpleDomain) minMax
    in CompositeDomain simpleSubdomains
