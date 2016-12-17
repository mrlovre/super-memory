module Pages where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Reader
import           Data.List
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.StateVar
import           Graphics.UI.Gtk        hiding (get)
import           Graphics.UI.Gtk.Gdk.GC

import           Process
import           Utility
import           Variables

createDrawingPage :: Notebook -> Variables -> IO ()
createDrawingPage notebook variables = void $ do
    mainBox <- hBoxNew False 6
    (notebook `notebookAppendPage` mainBox) "Drawing"

    frame <- frameNew
    (mainBox `boxPackStart` frame) PackGrow 0

    drawingArea <- drawingAreaNew
    frame `containerAdd` drawingArea

    vSeparator <- vSeparatorNew
    (mainBox `boxPackStart` vSeparator) PackNatural 0

    sideBox <- vBoxNew False 6
    (mainBox `boxPackStart` sideBox) PackNatural 0

    gestureEntryBox <- hBoxNew False 6
    (sideBox `boxPackStart` gestureEntryBox) PackNatural 0
    gestureEntry <- entryNew
    (gestureEntryBox `boxPackStart` gestureEntry) PackGrow 0
    gestureAddButton <- buttonNewFromStock stockAdd
    (gestureEntryBox `boxPackStart` gestureAddButton) PackNatural 0

    scrolledWindow <- scrolledWindowNew Nothing Nothing
    (sideBox `boxPackStart` scrolledWindow) PackGrow 0
    (scrolledWindow `scrolledWindowSetPolicy`) PolicyNever PolicyAutomatic

    scrolledWindowBox <- vBoxNew False 0
    scrolledWindow `scrolledWindowAddWithViewport` scrolledWindowBox

    (gestureAddButton `onClicked`) $ do
        text <- entryGetText gestureEntry
        gestureTabsV <- get $ gestureTabs variables
        unless (any ($ text) [null, (`S.member` M.keysSet gestureTabsV)]) $ do
            gestureEntry `entrySetText` ""
            createGestureBox text scrolledWindowBox variables
    buttonBox <- hButtonBoxNew
    (sideBox `boxPackStart` buttonBox) PackNatural 0
    buttonBox `set` [buttonBoxLayoutStyle := ButtonboxCenter]

    buttonOpen <- buttonNewWithLabel "Open"
    buttonBox `containerAdd` buttonOpen
    (buttonOpen `onClicked`) $ do
        fileChooserDialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen
                [(stockCancel, ResponseCancel), (stockOpen, ResponseAccept)]
        dialogRun fileChooserDialog
        result <- fileChooserGetFilename fileChooserDialog
        case result of
            Just fileName -> loadFile variables fileName
            Nothing -> return ()
        widgetDestroy fileChooserDialog

    buttonSave <- buttonNewWithLabel "Save"
    buttonBox `containerAdd` buttonSave
    (buttonSave `onClicked`) $ do
        fileChooserDialog <- fileChooserDialogNew Nothing Nothing FileChooserActionSave
                [(stockCancel, ResponseCancel), (stockOpen, ResponseAccept)]
        dialogRun fileChooserDialog
        result <- fileChooserGetFilename fileChooserDialog
        case result of
            Just fileName -> saveFile variables fileName
            Nothing -> return ()
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
            tempPointsV <- normalizeData . decimateData . nub <$> get (tempPoints variables)
            selectedGestureV <- get $ selectedGesture variables
            unless (null selectedGestureV) $ gestures variables $~ M.insertWith (++) selectedGestureV [tempPointsV]
            widgetShowAll notebook
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
        (drawWindow `drawLines` gc) $ decimateData tempPointsV
        return True

createTrainingPage :: Notebook -> Variables -> IO ()
createTrainingPage notebook variables = void $ do
    trainingHBox <- hBoxNew False 6
    (notebook `notebookAppendPage` trainingHBox) "Training"

    trainingVBox <- vBoxNew False 0
    (trainingHBox `boxPackStart` trainingVBox) PackRepel 0

    algorithmLabel <- labelNew $ Just "Algorithm:"
    (trainingVBox `boxPackStart` algorithmLabel) PackNatural 6

    algorithmComboBox <- comboBoxNewText
    (trainingVBox `boxPackStart` algorithmComboBox) PackNatural 6

    architectureLabel <- labelNew $ Just "Network architecture:"
    (trainingVBox `boxPackStart` architectureLabel) PackNatural 6

    architectureEntry <- entryNew
    (trainingVBox `boxPackStart` architectureEntry) PackNatural 6

    buttonBox <- hButtonBoxNew
    buttonBox `set` [buttonBoxLayoutStyle := ButtonboxCenter]
    (trainingVBox `boxPackStart` buttonBox) PackNatural 6

    trainButton <- buttonNewWithLabel "Train"
    buttonBox `containerAdd` trainButton

    testingBox <- vBoxNew False 6
    (notebook `notebookAppendPage` testingBox) "Testing"

createTestingPage :: Notebook -> Variables -> IO ()
createTestingPage notebook variables = return ()

createGestureBox :: BoxClass self => String -> self -> Variables -> IO ()
createGestureBox text parent variables = do
    gestureButton <- buttonNewWithLabel text
    gestureRemoveButton <- buttonNew
    trashImage <- imageNewFromStock stockDelete IconSizeButton
    gestureRemoveButton `buttonSetImage` trashImage
    gestureButtonBox <- hBoxNew False 0
    parent `containerAdd` gestureButtonBox
    (gestureButtonBox `boxPackStart` gestureButton) PackGrow 0
    (gestureButtonBox `boxPackStart` gestureRemoveButton) PackNatural 0

    (gestureButton `onClicked`) $ selectedGesture variables $= text
    (gestureButton `onDestroy`) $ do
        widgetDestroy gestureRemoveButton
        widgetDestroy gestureButtonBox
    (gestureRemoveButton `onClicked`) $ do
        widgetDestroy gestureButton
        gestures variables $~ M.delete text
        gestureTabs variables $~ M.delete text
    (parent `boxSetChildPacking` gestureButtonBox) PackNatural 0 PackStart
    gestureTabs variables $~ M.insert text gestureButton
    widgetShowAll parent
