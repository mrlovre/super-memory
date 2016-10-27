module Relations.Relation where

import           FuzzySets.FuzzySet

type Relation = AFuzzySet

relation :: FuzzySet a => a -> Relation
relation = AFuzzySet
