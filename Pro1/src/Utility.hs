module Utility where

-- | Creates a cartesian product of two sets, i.e. pairs each element of the first set with an element of the second
-- set, in order.
-- Equal to (without list comprehension): \ a b -> concatMap (\ i -> map (\ j -> i ++ j) b) a
cartesianProduct :: [[a]] -> [[a]] -> [[a]]
cartesianProduct a b = [i ++ j | i <- a, j <- b]
