{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Variables where

import           Data.Map        (Map)
import qualified Data.Map        as M
import           GHC.IORef
import           Graphics.UI.Gtk

type DecimalPoint = (Double, Double)

data Variables = Variables {
    tempPoints      :: IORef [Point],
    selectedGesture :: IORef String,
    gestures        :: IORef (Map String [[DecimalPoint]]),
    gestureTabs     :: IORef (Map String Button)
}

initVariables :: IO Variables
initVariables = do
    tempPoints <- newIORef []
    selectedGesture <- newIORef ""
    gestures <- newIORef M.empty
    gestureTabs <- newIORef M.empty
    return Variables { .. }
