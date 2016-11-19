module Evaluation where

import           Chromosome
import           Utility

evaluateDataset :: Dataset -> Chromosome -> Double
evaluateDataset ds c = mean $ map (\ (d, z) -> sqDist z $ evaluate c d) ds
