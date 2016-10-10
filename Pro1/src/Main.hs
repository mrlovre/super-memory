module Main where

import           Domain
import           DomainElement
import           DomainHelper

import           FuzzySet
import           FuzzySetHelper
import           FuzzySets.CalculatedFuzzySet
import           FuzzySets.MutableFuzzySet

import           Control.Arrow

main :: IO ()
main = demo1 >> demo2

demo1 :: IO ()
demo1 = do
    let domain1 = intRange 0 5
    let domain2 = intRange 0 3
    let domain3 = combine [domain1, domain2]
    putStrLn $ domainInfo domain1
    putStrLn $ domainInfo domain2
    putStrLn $ domainInfo domain3

    print $ elementAtIndex domain3 0
    print $ elementAtIndex domain3 5
    print $ elementAtIndex domain3 14
    print $ indexOfElement domain3 $ createDomainElement [4, 1]

demo2 :: IO ()
demo2 = do
    let domain1 = ADomain $ intRange 0 11
        set1 = updateMutableFuzzySet (mutableFuzzySet domain1)
            (map (first createDomainElement) [([0], 1), ([1], 0.8), ([2], 0.6), ([3], 0.4), ([4], 0.2)])
        domain2 = ADomain $ intRange (-5) 6
        [i1, i2, i3] = map (indexOfElement domain2 . createDomainElement) [[-4], [0], [4]]
        set2 = calculatedFuzzySet (lambdaFunctionGenerator i1 i2 i3) domain2
    print $ AFuzzySet set1
    print $ AFuzzySet set2

domainInfo :: (Domain a) => a -> String
domainInfo d = unlines $
    "Domain info:" :
    show (ADomain d) :
    ["Cardinality: " ++ show (cardinality d)]
