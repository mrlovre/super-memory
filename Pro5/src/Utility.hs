module Utility where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Random
import qualified Data.Map             as M
import           Data.StateVar
import           Debug.Trace

import           Variables

saveFile :: Variables -> FilePath -> IO ()
saveFile variables path = do
    gesturesV <- get $ gestures variables
    writeFile path $ M.foldWithKey (\ k v s -> s ++ unlines [show k, show v]) "" gesturesV

loadFile :: Variables -> FilePath -> IO ()
loadFile variables path = do
    file <- readFile path
    let (keys, values) = join (***) (map snd) $ break (even . fst) $ zip [1 :: Int ..] $ lines file
        newGestures = M.fromList $ zip keys (map read values)
    gestures variables $= newGestures
    return ()

randomPM1 :: (MonadRandom m) => m Double
randomPM1 = (+ (-1)) . (* 2) <$> getRandom

screen :: (Show a) => a -> a
screen = join traceShow

screenT :: (Show a) => String -> a -> a
screenT text value = let
    in (text ++ show value) `trace` value
