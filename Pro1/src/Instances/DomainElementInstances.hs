module Instances.DomainElementInstances where

import           Dimensionable
import           Domain
import           DomainElement

instance Dimensionable DomainElement where
    dimension (DomainElement a) = length a
