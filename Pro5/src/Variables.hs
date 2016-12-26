module Variables where

import           Control.Arrow
import           Control.Monad
import           Data.IORef
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.StateVar
import qualified Data.Vector     as V
import           Graphics.UI.Gtk hiding (get)

import           NeuralNetworks

type DecimalPoint = (Double, Double)

data Variables = Variables {
    tempPoints      :: IORef [Point],
    selectedGesture :: IORef String,
    gestures        :: IORef (Map String [[DecimalPoint]]),
    gestureTabs     :: IORef (Map String Button),
    neuralNetwork   :: IORef NN
}

initVariables :: IO Variables
initVariables = do
    tempPoints <- newIORef []
    selectedGesture <- newIORef ""
    gestures <- newIORef M.empty
    gestureTabs <- newIORef M.empty
    neuralNetwork <- newIORef $ NN V.empty
    return Variables { .. }

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
