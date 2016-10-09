module Domains.SimpleDomain where

-- | Model of a simple domain which consists of single one-dimensional int range.
data SimpleDomain = SimpleDomain Int Int

-- | Constructor for 'SimpleDomain'.
createSimpleDomain :: Int -> Int -> SimpleDomain
createSimpleDomain start end = if
    | start > end -> error "Domain start must be less than or equal to the domain end."
    | otherwise -> SimpleDomain start end

-- | Creates a 'SimpleDomain' containing range from a (inclusive) to b (exclusive).
intRange :: Int -> Int -> SimpleDomain
intRange a b = createSimpleDomain a (b - 1)
