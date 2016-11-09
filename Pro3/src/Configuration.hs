module Configuration where

import           FuzzySets.Operations

sNorm :: BinaryFunction
sNorm = zadehOr

tNorm :: BinaryFunction
tNorm = zadehAnd
