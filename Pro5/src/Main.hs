module Main where

import           Graphics.UI.Gtk

import           Pages
import           Variables

import Test

default (Double, Double)

main :: IO ()
main = do
    variables <- initVariables
    initGUI

    window <- windowNew
    window `set` [windowTitle := "Lab5"]

    notebook <- notebookNew
    window `containerAdd` notebook

    createDrawingPage  notebook variables
    createTrainingPage notebook variables
    createTestingPage  notebook variables

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
