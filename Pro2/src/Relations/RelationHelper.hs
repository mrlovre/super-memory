module Relations.RelationHelper where

import           Domains.Domain
import           Domains.DomainHelper
import           Domains.Dimensionable
import           Domains.DomainElement
import           FuzzySets.FuzzySet
import           FuzzySets.CalculatedFuzzySet
import           Relations.Relation
import           Utility

isUTimesURelation :: Relation -> Bool
isUTimesURelation r = let
    d = domain r
    components = getAllComponents d
    in case components of
        [c1, c2] -> c1 == c2
        _        -> False

isSymmetric :: Relation -> Bool
isSymmetric r = let
    pairs = extractDomainPairs r
    in isUTimesURelation r && all (\ (e1, e2) -> f (joinElements e1 e2) == f (joinElements e2 e1)) pairs where
        f = valueAt r

isReflexive :: Relation -> Bool
isReflexive r = let
    d = domain r
    dim = cardinality $ getComponent d 0
    is = diagonalIndices dim
    elements = map (elementAtIndex d) is
    in isUTimesURelation r && all (== 1) (map (valueAt r) elements)

isMaxMinTransitive :: Relation -> Bool
isMaxMinTransitive r = let
    it = domainIterator r
    pairs = extractDomainPairs r
    maxMin x y = maximum $ map (\ z -> min (f (joinElements x z)) (f (joinElements z y))) it
    f = valueAt r
    in isUTimesURelation r && all (\ (x, y) -> f (joinElements x y) >= maxMin x y) pairs

compositionOfBinaryRelations :: Relation -> Relation -> Relation
compositionOfBinaryRelations r1 r2 = let
    [d1, d2] = map domain [r1, r2]
    [c11, c12] = map (getComponent d1) [0, 1]
    [c21, c22] = map (getComponent d2) [0, 1]
    d = ADomain $ combine [c11, c22]
    mu i = let
        [x, y] = map (domainElement . (:[])) $ extractDomainElement $ elementAtIndex d i
        in maximum $ map (\ z -> min (valueAt r1 (joinElements x z)) (valueAt r2 (joinElements z y))) (iterator c12)
    in if
        | all isBinaryRelation [r1, r2] && c12 == c21 -> relation $ calculatedFuzzySet mu d
        | otherwise -> error "Only binary relations with equal inner domains may be composed."

isFuzzyEquivalence :: Relation -> Bool
isFuzzyEquivalence r = all ($ r) [isSymmetric, isReflexive, isMaxMinTransitive]

isBinaryRelation :: AFuzzySet -> Bool
isBinaryRelation r = dimension (domain r) == 2

domainIterator :: Relation -> [DomainElement]
domainIterator r = let
    d = domain r
    c = getComponent d 0
    in iterator c

extractDomainPairs :: Relation -> [(DomainElement, DomainElement)]
extractDomainPairs r = let
    it = domainIterator r
    in [(p1, p2) | p1 <- it, p2 <- it]
