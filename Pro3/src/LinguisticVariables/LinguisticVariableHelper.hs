module LinguisticVariables.LinguisticVariableHelper where

import           FuzzySets.FuzzySet
import           FuzzySets.FuzzySetHelper
import           FuzzySets.Operations

lvNot :: AFuzzySet -> AFuzzySet
lvNot = (`unaryOperation` zadehNot)

lvVery :: AFuzzySet -> AFuzzySet
lvVery = (`unaryOperation` (** 2))

lvMoreOrLess :: AFuzzySet -> AFuzzySet
lvMoreOrLess = (`unaryOperation` (** 0.5))
