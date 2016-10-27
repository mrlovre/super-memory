module Main where

import           Control.Arrow
import           Control.Monad

import           Domains.Domain
import           Domains.DomainElement
import           Domains.DomainHelper
import           FuzzySets.FuzzySet
import           FuzzySets.MutableFuzzySet
import           Relations.Relation
import           Relations.RelationHelper
import           Utility

main :: IO ()
main = demo1 >> demo2 >> demo3

demo1 :: IO ()
demo1 = do
    putStrLn "Demo 1"
    putStrLn ""
    let u = ADomain $ intRange 1 6
        uxu = ADomain $ combine [u, u]
        r1Mu = map (first domainElement) [
            ([1, 1], 1),
            ([2, 2], 1),
            ([3, 3], 1),
            ([4, 4], 1),
            ([5, 5], 1),
            ([3, 1], 0.5),
            ([1, 3], 0.5)]
        r2Mu = map (first domainElement) [
            ([1, 1], 1),
            ([2, 2], 1),
            ([3, 3], 1),
            ([4, 4], 1),
            ([5, 5], 1),
            ([3, 1], 0.5),
            ([1, 3], 0.1)]
        r3Mu = map (first domainElement) [
            ([1, 1], 1),
            ([2, 2], 1),
            ([3, 3], 0.3),
            ([4, 4], 1),
            ([5, 5], 1),
            ([1, 2], 0.6),
            ([2, 1], 0.6),
            ([2, 3], 0.7),
            ([3, 2], 0.7),
            ([1, 3], 0.5),
            ([3, 1], 0.5)]
        r4Mu = map (first domainElement) [
            ([1, 1], 1),
            ([2, 2], 1),
            ([3, 3], 1),
            ([4, 4], 1),
            ([5, 5], 1),
            ([1, 2], 0.4),
            ([2, 1], 0.4),
            ([2, 3], 0.5),
            ([3, 2], 0.5),
            ([1, 3], 0.4),
            ([3, 1], 0.4)]
        [r1, r2, r3, r4] = map (relation . updateMutableFuzzySet (mutableFuzzySet uxu)) [r1Mu, r2Mu, r3Mu, r4Mu]
    putStrLn "R1 je definirana nad UxU?"
    print $ isUTimesURelation r1
    putStrLn "R1 je simetrična?"
    print $ isSymmetric r1
    putStrLn "R2 je simetrična?"
    print $ isSymmetric r2
    putStrLn "R1 je refleksivna?"
    print $ isReflexive r1
    putStrLn "R3 je refleksivna?"
    print $ isReflexive r3
    putStrLn "R3 je max-min tranzitivna?"
    print $ isMaxMinTransitive r3
    putStrLn "R4 je max-min tranzitivna?"
    print $ isMaxMinTransitive r4
    putStrLn ""

demo2 :: IO ()
demo2 = do
    putStrLn "Demo 2"
    putStrLn ""
    let u1 = ADomain $ intRange 1 5
        u2 = ADomain $ intRange 1 4
        u3 = ADomain $ intRange 1 5
        u1u2 = ADomain $ combine [u1, u2]
        u2u3 = ADomain $ combine [u2, u3]
        r1Mu = map (first domainElement) [
            ([1, 1], 0.3),
            ([1, 2], 1),
            ([3, 3], 0.5),
            ([4, 3], 0.5)]
        r1 = relation $ updateMutableFuzzySet (mutableFuzzySet u1u2) r1Mu
        r2Mu = map (first domainElement) [
            ([1, 1], 1),
            ([2, 1], 0.5),
            ([2, 2], 0.7),
            ([3, 3], 1),
            ([3, 4], 0.4)]
        r2 = relation $ updateMutableFuzzySet (mutableFuzzySet u2u3) r2Mu
        r1r2 = compositionOfBinaryRelations r1 r2
    forM_ (iterator $ domain r1r2) $ \ e -> do
        let string = "mu(" ++ show e ++ ")=" ++ show (valueAt r1r2 e)
        putStrLn string
    putStrLn ""

demo3 :: IO ()
demo3 = do
    putStrLn "Demo 3"
    putStrLn ""
    let u = ADomain $ intRange 1 5
        uxu = ADomain $ combine [u, u]
        mu = map (first domainElement) [
            ([1,1], 1),
            ([2,2], 1),
            ([3,3], 1),
            ([4,4], 1),
            ([1,2], 0.3),
            ([2,1], 0.3),
            ([2,3], 0.5),
            ([3,2], 0.5),
            ([3,4], 0.2),
            ([4,3], 0.2)]
        r = relation $ updateMutableFuzzySet (mutableFuzzySet uxu) mu
        compositions = takeWhileIncluding (not . isFuzzyEquivalence) $ iterate (compositionOfBinaryRelations r) r
    relationInfo r
    putStrLn "Početna neizrazita relacija je neizrazita relacija ekvivalencije?"
    print $ isFuzzyEquivalence r
    forM_ (enumerate compositions) $ \ (i, r') -> do
        putStrLn $ show i ++ ". iteracija:"
        relationInfo r'
        putStrLn "Neizrazita relacije je neizrazita relacija ekvivalencije?"
        print $ isFuzzyEquivalence r'

relationInfo :: Relation -> IO ()
relationInfo r =
    forM_ (iterator $ domain r) $ \ e -> do
        let string = "mu(" ++ show e ++ ")=" ++ show (valueAt r e)
        putStrLn string
