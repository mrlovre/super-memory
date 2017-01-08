module Utility where

import           Control.Monad.Random


meshGrid :: [a] -> [a] -> [(a, a)]
meshGrid xs ys = concatMap ((`zip` ys) . repeat) xs

randomDouble :: IO Double
randomDouble = (subtract 1 . (* 2)) <$> getRandom

iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m [a]
iterateUntilM p r a = if
    | p a -> return [a]
    | otherwise -> do
        a' <- r a
        (a :) <$> (iterateUntilM p r $! a')
