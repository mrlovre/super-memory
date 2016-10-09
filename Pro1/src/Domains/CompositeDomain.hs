module Domains.CompositeDomain where

import           Domains.SimpleDomain

-- | Model of a composite domain which is a cartesian product of several 'SimpleDomain's.
data CompositeDomain where
    CompositeDomain :: [SimpleDomain] -> CompositeDomain

-- | Constructor for 'CompositeDomain'.
createCompositeDomain :: [SimpleDomain] -> CompositeDomain
createCompositeDomain [] = error "Composite domain must contain at least one component."
createCompositeDomain other = CompositeDomain other
