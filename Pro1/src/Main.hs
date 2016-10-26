-- | The main module.
module Main where

import           Domains.Domain
import           Domains.DomainElement
import           Domains.DomainHelper

import           FuzzySets.CalculatedFuzzySet
import           FuzzySets.FuzzySet
import           FuzzySets.FuzzySetHelper
import           FuzzySets.MutableFuzzySet
import           FuzzySets.Operations

import           Control.Arrow

-- | The main function.
main :: IO ()
main = demo1 >> demo2 >> demo3

-- | Demonstration number one.
demo1 :: IO ()
demo1 = do
    putStrLn "Demo 1#"
    let domain1 = intRange 0 5
    let domain2 = intRange 0 3
    let domain3 = combine [domain1, domain2]
    putStrLn $ domainInfo domain1
    putStrLn $ domainInfo domain2
    putStrLn $ domainInfo domain3

    print $ elementAtIndex domain3 0
    print $ elementAtIndex domain3 5
    print $ elementAtIndex domain3 14
    print $ indexOfElement domain3 $ domainElement [4, 1]

-- | Demonstration number two.
demo2 :: IO ()
demo2 = do
    putStrLn "Demo 2#"
    let domain1 = ADomain $ intRange 0 11
        set1 = updateMutableFuzzySet (mutableFuzzySet domain1) $
            map (first domainElement) [([0], 1), ([1], 0.8), ([2], 0.6), ([3], 0.4), ([4], 0.2)]
        domain2 = ADomain $ intRange (-5) 6
        [i1, i2, i3] = map (indexOfElement domain2 . domainElement) [[-4], [0], [4]]
        set2 = calculatedFuzzySet (lambdaFunctionGenerator i1 i2 i3) domain2
    print $ AFuzzySet set1
    print $ AFuzzySet set2

-- | Demonstration number three.
demo3 :: IO ()
demo3 = do
    putStrLn "Demo 3#"
    let d = ADomain $ intRange 0 11
        set1 = AFuzzySet $ updateMutableFuzzySet (mutableFuzzySet d) $
            map (first domainElement) [([0], 1), ([1], 0.8), ([2], 0.6), ([3], 0.4), ([4], 0.2)]
        notSet1 = unaryOperation set1 zadehNot
        union = binaryOperation set1 notSet1 zadehOr
        hamacherIntersection = binaryOperation set1 notSet1 $ hamacherProductParam 1
    print set1
    print notSet1
    print union
    print hamacherIntersection

-- | Provides debug info for a domain.
domainInfo :: (Domain a) => a -> String
domainInfo d = unlines $
    "Domain info:" :
    show (ADomain d) :
    ["Cardinality: " ++ show (cardinality d)]
