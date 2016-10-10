module Domains.Dimensionable where

-- | Defines a class with a dimension property.
class Dimensionable a where
    -- | Returns the dimension of a domain element.
    dimension :: a -> Int
