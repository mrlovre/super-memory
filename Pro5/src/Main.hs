module Main (main) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans
import           Data.IORef
import           Data.StateVar
import           Graphics.UI.Gtk        hiding (get)
import           Graphics.UI.Gtk.Gdk.GC
import           Data.List

import           Data.Map (Map)
import qualified Data.Map as M

default (Double, Double)

type DecimalPoint = (Double, Double)

data Variables = Variables {
    tempPoints :: IORef [Point],
    selectedGesture :: IORef String,
    gestures :: IORef (Map String [DecimalPoint])
}

initVariables :: IO Variables
initVariables = do
    tempPoints <- newIORef []
    selectedGesture <- newIORef ""
    gestures <- newIORef M.empty
    return Variables { .. }

main :: IO ()
main = do
    variables <- initVariables
    initGUI

    window <- windowNew
    notebook <- notebookNew

    mainBox <- hBoxNew False 6
    (notebook `notebookAppendPage` mainBox) "Drawing"

    drawingArea <- drawingAreaNew
    mainBox `containerAdd` drawingArea

    drawingArea2 <- drawingAreaNew
    (notebook `notebookAppendPage` drawingArea2) "Drawing2"

    vSeparator <- vSeparatorNew
    mainBox `containerAdd` vSeparator

    sideBox <- vBoxNew True 6
    mainBox `containerAdd` sideBox
    mainBox `set` [boxChildPacking vSeparator := PackNatural, boxChildPacking sideBox := PackNatural]

    button <- buttonNewWithLabel "Click me."
    sideBox `containerAdd` button
    sideBox `set` [boxChildPacking button := PackNatural]
    (button `onClicked`) $
        putStrLn "Clicked"
    --widgetSetSizeRequest button 50 50

    button2 <- buttonNewWithLabel "Do not click me."
    (button2 `onClicked`) $ do
        fileChooserDialog <- fileChooserDialogNew Nothing (Just window) FileChooserActionOpen [(stockCancel, ResponseCancel), (stockOpen, ResponseAccept)]
        dialogRun fileChooserDialog
        result <- fileChooserGetFilename fileChooserDialog
        case result of
            Just fileName -> putStrLn fileName
            Nothing -> putStrLn "Nije ništa izabrano."
        widgetDestroy fileChooserDialog
    sideBox `containerAdd` button2
    sideBox `set` [boxChildPacking button2 := PackGrow]

    (drawingArea `widgetSetSizeRequest`) 600 600
    window `set` [windowTitle := "Lab5", containerChild := notebook]
    drawingArea `set` [widgetCanFocus := True]
    drawingArea `widgetSetEvents` [ButtonPressMask, ButtonMotionMask, PointerMotionHintMask]
    drawingArea `on` buttonPressEvent $ do
        liftIO $ tempPoints variables $= []
        return True
    drawingArea `on` buttonReleaseEvent $ do
        liftIO $ do
            tempPointsV <- normalizeData . nub <$> get (tempPoints variables)
            selectedGestureV <- get $ selectedGesture variables
            gestures variables $~ M.insertWith (++) selectedGestureV tempPointsV
            drawingAreaSample <- drawingAreaNew
            (drawingAreaSample `widgetSetSizeRequest`) 20 20
            sideBox `set` [boxChildPacking drawingAreaSample := PackNatural]
            sideBox `containerAdd` drawingAreaSample
            drawWindow <- widgetGetDrawWindow drawingArea2
            drawWindowClear drawWindow
            gc <- gcNew drawWindow
            (drawWindow `drawLines` gc) $ scaleData 20 20 tempPointsV
        return True
    drawingArea `on` motionNotifyEvent $ do
        (x, y) <- join (***) round <$> eventCoordinates
        liftIO $ do
            tempPoints variables $~ (++ [(x, y)])
            drawWindow <- widgetGetDrawWindow drawingArea
            gc <- gcNew drawWindow
            drawWindowClear drawWindow
            get (tempPoints variables) >>= drawWindow `drawLines` gc
        eventRequestMotions
        return True

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

normalizeData :: [Point] -> [DecimalPoint]
normalizeData points = let
    (m, n) = join (***) fromIntegral . (maximum &&& minimum) $ concatMap flattenTuple points :: (Double, Double)
    flattenTuple (a, b) = [a, b]
    (meanX, meanY) = join (***) fromIntegral $ foldl (\ (a, b) (c, d) -> (a + c, b + d)) (0, 0) points :: (Double, Double)
    in map (\ (x, y) -> ((fromIntegral x - meanX) / (m - n), (fromIntegral y - meanY) / (m - n))) points

scaleData :: Double -> Double -> [DecimalPoint] -> [Point]
scaleData scale offset =
    map (join (***) (round . (+ offset) . (* scale)))
