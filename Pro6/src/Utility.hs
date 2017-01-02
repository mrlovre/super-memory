module Utility where

import           Control.Monad.Random


meshGrid :: [a] -> [a] -> [(a, a)]
meshGrid xs ys = concatMap ((`zip` ys) . repeat) xs

randomDouble :: IO Double
randomDouble = (subtract 1 . (* 2)) <$> getRandom
