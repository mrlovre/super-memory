module FuzzySystems.FuzzySystemHelper where

import           Data.Map (Map)
import qualified Data.Map as Map

type Variables = Map String Int

variables :: [(String, Int)] -> Variables
variables = Map.fromList

variablesForBoatSystem :: [Int] -> Variables
variablesForBoatSystem = variables . zip ["l", "d", "lk", "dk", "v", "s"]

getVariable :: Variables -> String -> Int
getVariable = flip (Map.findWithDefault (error "Variable is not present in dictionary."))
