-- | Domain element.
module Domains.DomainElement where

import           Domains.Dimensionable

-- | Model of a multi-dimension domain element.
newtype DomainElement = DomainElement [Int] deriving (Eq)

-- | Constructor for 'DomainElement'.
domainElement :: [Int] -> DomainElement
domainElement [] = error "Domain element must contain at least one component."
domainElement a  = DomainElement a

-- | Deconstructor for 'DomainElement'.
extractDomainElement :: DomainElement -> [Int]
extractDomainElement (DomainElement e) = e

-- | Retrieves n-th dimension value from a domain element.
getComponentValue :: Int -> DomainElement -> Int
getComponentValue n (DomainElement a) = a !! n

-- | Joins two elements into a composite element.
joinElements :: DomainElement -> DomainElement -> DomainElement
joinElements (DomainElement a) (DomainElement b) = DomainElement (a ++ b)

-- | Show instance.
instance Show DomainElement where
    show (DomainElement a) = show a

-- | Dimensionable instance.
instance Dimensionable DomainElement where
    dimension (DomainElement a) = length a
