-- | Domain helper functions.
module Domains.DomainHelper where

import           Domains.CompositeDomain
import           Domains.Domain
import           Domains.DomainElement
import           Domains.SimpleDomain

import           Control.Arrow
import           Control.Monad

-- | Creates a 'SimpleDomain' containing range from a (inclusive) to b (exclusive).
intRange :: Int -> Int -> SimpleDomain
intRange a b = simpleDomain a (b - 1)

-- | Creates a 'CompositeDomain' as a CartesianProduct of supplied domains.
combine :: (Domain a) => [a] -> CompositeDomain
combine domains = let
    components = concatMap getAllComponents domains
    minMax = map (join (***) (head . extractDomainElement) . (minElement &&& maxElement)) components
    simpleSubdomains = map (uncurry SimpleDomain) minMax
    in CompositeDomain simpleSubdomains
