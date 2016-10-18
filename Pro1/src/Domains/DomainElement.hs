-- | Domain element.
module Domains.DomainElement where

import           Domains.Dimensionable

-- | Model of a multi-dimension domain element.
newtype DomainElement = DomainElement [Int] deriving (Eq)

-- | Constructor for 'DomainElement'.
createDomainElement :: [Int] -> DomainElement
createDomainElement [] = error "Domain element must contain at least one component."
createDomainElement a  = DomainElement a

-- | Deconstructor for 'DomainElement'.
extractDomainElement :: DomainElement -> [Int]
extractDomainElement (DomainElement e) = e

-- | Retrieves n-th dimension value from a domain element.
getComponentValue :: Int -> DomainElement -> Int
getComponentValue n (DomainElement a) = a !! n

-- | Show instance.
instance Show DomainElement where
    show (DomainElement a) = show a

-- | Dimensionable instance.
instance Dimensionable DomainElement where
    dimension (DomainElement a) = length a