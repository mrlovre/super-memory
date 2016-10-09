module Main where

import           Domain
import           DomainElement
import           DomainHelper
import           Instances     ()

main :: IO ()
main = putStrLn "Hello, Haskell!"

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

domainInfo :: (Domain a) => a -> String
domainInfo domain = unlines $
    "Domain info:" :
    show (ADomain domain) :
    ["Cardinality: " ++ show (cardinality domain)]
