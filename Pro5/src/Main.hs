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
import Data.Set(Set)
import qualified Data.Set as S

default (Double, Double)

type DecimalPoint = (Double, Double)

data Variables = Variables {
    tempPoints :: IORef [Point],
    selectedGesture :: IORef String,
    gestures :: IORef (Map String [DecimalPoint]),
    gestureTabs :: IORef (Map String Button)
}

initVariables :: IO Variables
initVariables = do
    tempPoints <- newIORef []
    selectedGesture <- newIORef ""
    gestures <- newIORef M.empty
    gestureTabs <- newIORef M.empty
    return Variables { .. }

main :: IO ()
main = do
    variables <- initVariables
    initGUI

    window <- windowNew
    window `set` [windowTitle := "Lab5"]

    notebook <- notebookNew
    window `containerAdd` notebook

    mainBox <- hBoxNew False 6
    (notebook `notebookAppendPage` mainBox) "Drawing"

    frame <- frameNew
    mainBox `containerAdd` frame

    drawingArea <- drawingAreaNew
    frame `containerAdd` drawingArea

    vSeparator <- vSeparatorNew
    mainBox `containerAdd` vSeparator

    sideBox <- vBoxNew False 6
    mainBox `containerAdd` sideBox

    mainBox `set` [boxChildPacking drawingArea := PackGrow,
                   boxChildPacking vSeparator := PackNatural,
                   boxChildPacking sideBox := PackNatural]

    gestureEntryBox <- hBoxNew False 6
    sideBox `containerAdd` gestureEntryBox
    gestureEntry <- entryNew
    gestureEntryBox `containerAdd` gestureEntry
    gestureAddButton <- buttonNewFromStock stockAdd
    gestureEntryBox `containerAdd` gestureAddButton
    gestureEntryBox `set` [boxChildPacking gestureEntry := PackGrow,
                           boxChildPacking gestureAddButton := PackNatural]

    scrolledWindow <- scrolledWindowNew Nothing Nothing
    sideBox `containerAdd` scrolledWindow
    (scrolledWindow `scrolledWindowSetPolicy`) PolicyNever PolicyAutomatic

    scrolledWindowBox <- vBoxNew False 0
    scrolledWindow `scrolledWindowAddWithViewport` scrolledWindowBox

    (gestureAddButton `onClicked`) $ do
        text <- entryGetText gestureEntry
        gestureTabsV <- get $ gestureTabs variables
        unless (any ($ text) [null, (`S.member` M.keysSet gestureTabsV)]) $ do
            gestureButton <- buttonNewWithLabel text
            gestureEntry `entrySetText` ""
            gestureRemoveButton <- buttonNew
            trashImage <- imageNewFromStock stockDelete IconSizeButton
            gestureRemoveButton `buttonSetImage` trashImage
            gestureButtonBox <- hBoxNew False 0
            scrolledWindowBox `containerAdd` gestureButtonBox
            gestureButtonBox `containerAdd` gestureButton
            gestureButtonBox `containerAdd` gestureRemoveButton
            gestureButtonBox `set` [boxChildPacking gestureButton := PackGrow,
                                    boxChildPacking gestureRemoveButton := PackNatural]
            (gestureButton `onClicked`) $ selectedGesture variables $= text
            (gestureButton `onDestroy`) $ do
                widgetDestroy gestureRemoveButton
                widgetDestroy gestureButtonBox
            (gestureRemoveButton `onClicked`) $ do
                widgetDestroy gestureButton
                gestures variables $~ M.delete text
                gestureTabs variables $~ M.delete text
            (scrolledWindowBox `boxSetChildPacking` gestureButtonBox) PackNatural 0 PackStart
            gestureTabs variables $~ M.insert text gestureButton
            widgetShowAll scrolledWindowBox
    buttonBox <- hButtonBoxNew
    sideBox `containerAdd` buttonBox
    buttonBox `set` [buttonBoxLayoutStyle := ButtonboxCenter]

    sideBox `set` [boxChildPacking gestureEntryBox := PackNatural,
                   boxChildPacking scrolledWindow := PackGrow,
                   boxChildPacking buttonBox := PackNatural]

    buttonOpen <- buttonNewWithLabel "Open"
    buttonBox `containerAdd` buttonOpen
    (buttonOpen `onClicked`) $
        putStrLn "Clicked"

    buttonSave <- buttonNewWithLabel "Save"
    buttonBox `containerAdd` buttonSave
    (buttonSave `onClicked`) $ do
        fileChooserDialog <- fileChooserDialogNew Nothing (Just window) FileChooserActionOpen
                [(stockCancel, ResponseCancel), (stockOpen, ResponseAccept)]
        dialogRun fileChooserDialog
        result <- fileChooserGetFilename fileChooserDialog
        case result of
            Just fileName -> putStrLn fileName
            Nothing -> putStrLn "Nije ništa izabrano."
        widgetDestroy fileChooserDialog

    (drawingArea `widgetSetSizeRequest`) 600 600
    (drawingArea `widgetModifyBg` StateNormal) (Color 65535 65535 65535)
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
            buttonx <- buttonNewWithLabel "TEST"
            scrolledWindowBox `containerAdd` buttonx
            widgetShowAll window
        return True

    drawingArea `on` motionNotifyEvent $ do
        (x, y) <- join (***) round <$> eventCoordinates
        liftIO $ do
            tempPoints variables $~ (++ [(x, y)])
            widgetQueueDraw drawingArea
        eventRequestMotions
        return True

    (drawingArea `onExpose`) $ const $ do
        drawWindow <- widgetGetDrawWindow drawingArea
        drawWindowClear drawWindow
        gc <- gcNew drawWindow
        tempPointsV <- get (tempPoints variables)
        (drawWindow `drawLines` gc) tempPointsV
        return True

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

normalizeData :: [Point] -> [DecimalPoint]
normalizeData points = let
    (m, n) = join (***) fromIntegral . (maximum &&& minimum) $ concatMap flattenTuple points
    flattenTuple (a, b) = [a, b]
    (meanX, meanY) = join (***) fromIntegral $ foldl (\ (a, b) (c, d) -> (a + c, b + d)) (0, 0) points
    in map (\ (x, y) -> ((fromIntegral x - meanX) / (m - n), (fromIntegral y - meanY) / (m - n))) points

scaleData :: Double -> Double -> [DecimalPoint] -> [Point]
scaleData scale offset =
    map (join (***) (round . (+ offset) . (* scale)))
