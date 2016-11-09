module Main where

import           Defuzzifiers.COADefuzzifier
import           Defuzzifiers.Defuzzifier
import           FuzzySystems.AccelFuzzySystemMin
import           FuzzySystems.FuzzySystem
import           FuzzySystems.FuzzySystemHelper
import           FuzzySystems.RudderFuzzySystemMin

import           Control.Monad
import           Utility

main :: IO ()
main = do
    let defuzzifier = ADefuzzifier coaDefuzzifier
        fsAccel = AFuzzySystem $ accelFuzzySystemMin defuzzifier
        fsRudder = AFuzzySystem $ rudderFuzzySystemMin defuzzifier
    forever $ do
        inputs <- variablesForBoatSystem <$> readInts
        --let outputs = map (`conclude` inputs) [fsAccel, fsRudder]
        let outputs = [0, 0]
        putStrLn $ unwords $ map show outputs
        undefined