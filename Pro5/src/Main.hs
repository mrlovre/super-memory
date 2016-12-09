module Main (main) where

import           Control.Arrow
import           Control.Monad.Trans
import           Data.IORef
import           Data.StateVar
import           Graphics.UI.Gtk        hiding (get)
import           Graphics.UI.Gtk.Gdk.GC

main :: IO ()
main = do
    tempPoints <- newIORef ([] :: [Point])
    _ <- initGUI
    window <- windowNew
    notebook <- notebookNew

    drawingArea <- drawingAreaNew
    _ <- (notebook `notebookAppendPage` drawingArea) "Drawing"
    widgetSetSizeRequest drawingArea 600 600
    window `set` [windowTitle := "Lab5", containerChild := notebook]
    drawingArea `set` [widgetCanFocus := True]
    drawingArea `widgetSetEvents` [ButtonPressMask, ButtonMotionMask, PointerMotionHintMask]
    _ <- drawingArea `on` buttonPressEvent $ do
        liftIO $ tempPoints $= []
        return True
    _ <- drawingArea `on` buttonReleaseEvent $ do
        liftIO $ get tempPoints >>= print
        return True
    _ <- drawingArea `on` motionNotifyEvent $ do
        (x, y) <- (round *** round) <$> eventCoordinates
        liftIO $ do
            print (x, y)
            tempPoints $~ (++ [(x, y)])
            drawWindow <- widgetGetDrawWindow drawingArea
            gc <- gcNew drawWindow
            drawWindowClear drawWindow
            get tempPoints >>= drawLines drawWindow gc
        eventRequestMotions
        return True

    drawingArea2 <- drawingAreaNew
    _ <- (notebook `notebookAppendPage` drawingArea2) "Drawing2"

    _ <- onDestroy window mainQuit
    widgetShowAll window
    mainGUI
