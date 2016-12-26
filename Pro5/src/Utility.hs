module Utility where

import           Control.Monad
import           Control.Monad.Random
import           Debug.Trace
import           Text.Regex

randomPM1 :: (MonadRandom m) => m Double
randomPM1 = (+ (-1)) . (* 2) <$> getRandom

screen :: (Show a) => a -> a
screen = join traceShow

screenT :: (Show a) => String -> a -> a
screenT text value = let
    in (text ++ show value) `trace` value

splitNumbers :: String -> [Int]
splitNumbers = map read . splitRegex (mkRegex "[^0-9]+")
