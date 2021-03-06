-- | Simple domain.
module Domains.SimpleDomain where

import Domains.Domain
import Domains.Dimensionable
import Domains.DomainElement

-- | Model of a simple domain which consists of single one-dimensional int range.
data SimpleDomain = SimpleDomain Int Int

-- | Constructor for 'SimpleDomain'.
simpleDomain :: Int -> Int -> SimpleDomain
simpleDomain start end = if
    | start > end -> error "Domain start must be less than or equal to the domain end."
    | otherwise -> SimpleDomain start end

-- | Creates a 'SimpleDomain' containing range from a (inclusive) to b (exclusive).
intRange :: Int -> Int -> SimpleDomain
intRange a b = simpleDomain a (b - 1)

-- | Domain instance.
instance Domain SimpleDomain where
    cardinality (SimpleDomain start end) = end - start + 1

    getComponent domain 0 = ADomain domain
    getComponent _ _      = error "Simple domain contains only one component."

    indexOfElement domain@(SimpleDomain start end) de = let
        e = getComponentValue 0 de
        in if
            | dimension de /= dimension domain ->
                error "Domain element is not of equal dimension as the domain."
            | e < start || e > end -> error "Domain element is not contained in the domain."
            | otherwise -> e - start

    elementAtIndex domain@(SimpleDomain start _) n = if
        | n < 0 || n >= cardinality domain -> error "Index exceeds domain boundaries."
        | otherwise -> DomainElement [start + n]

    iterator (SimpleDomain start end) = map (DomainElement . (:[])) [start .. end]

    minElement (SimpleDomain start _) = DomainElement [start]

    maxElement (SimpleDomain _ end) = DomainElement [end]

-- | Dimensionable instance.
instance Dimensionable SimpleDomain where
    dimension _ = 1
