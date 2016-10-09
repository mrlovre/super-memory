module Instances.SimpleDomainInstances where

import           Dimensionable
import           Domain
import           Domains.SimpleDomain
import           DomainElement
import           Instances.DomainElementInstances

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
