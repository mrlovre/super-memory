module Main where

import           Defuzzifiers.COADefuzzifier
import           Defuzzifiers.Defuzzifier
import           FuzzySystems.AccelFuzzySystemMin
import           FuzzySystems.FuzzySystem
import           FuzzySystems.FuzzySystemHelper
import           FuzzySystems.RudderFuzzySystemMin

import           Control.Monad
import           System.Exit
import           System.IO

main :: IO ()
main = do
    let defuzzifier = ADefuzzifier coaDefuzzifier
        fsAccel = AFuzzySystem $ accelFuzzySystemMin defuzzifier
        fsRudder = AFuzzySystem $ rudderFuzzySystemMin defuzzifier
    forever $ do
        line <- getLine
        when (line == "KRAJ") exitSuccess
        let inputs = variablesForBoatSystem $ map read $ words line
            outputs = map (`conclude` inputs) [fsAccel, fsRudder]
        putStrLn $ unwords $ map show outputs
        hFlush stdout
