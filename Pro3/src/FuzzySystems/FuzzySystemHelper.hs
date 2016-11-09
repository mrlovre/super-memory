module FuzzySystems.FuzzySystemHelper where

import           Data.Map (Map)
import qualified Data.Map as Map

type Variables = Map String Double

variables :: [(String, Double)] -> Variables
variables = Map.fromList

variablesForBoatSystem :: [Double] -> Variables
variablesForBoatSystem = variables . zip ["l", "d", "lk", "dk", "v", "s"]
