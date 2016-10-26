module Relations.RelationHelper where

import           Domains.Dimensionable
import           Domains.Domain
import           Domains.DomainElement
import           FuzzySets.FuzzySet
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
    pairs = extractDomainPairs
    in isUTimesURelation r && all (\ (e1, e2) -> f (joinElements e1 e2) == f (joinElements e2 e1)) pairs where
        f = valueAt r

isReflexive :: Relation -> Bool
isReflexive r = let
    d = domain r
    dim = dimension $ getComponent d 0
    is = diagonalIndices dim
    elements = map (elementAtIndex d) is
    in isUTimesURelation r && all (== 1) (map (valueAt r) elements)

isMaxMinTransitive :: Relation -> Bool
isMaxMinTransitive r = let
    d = domain r
    c = getComponent d 0
    pairs = [(p1, p2) | p1 <- iterator c, p2 <- iterator c]
    maxMin x y = maximum $ map (\ z -> min (f (joinElements x z)) (f (joinElements z y))) (iterator c)
    f = valueAt r
    in isUTimesURelation r && all (\ (x, y) -> f (joinElements x y) >= maxMin x y) pairs

compositionOfBinaryRelations :: Relation -> Relation -> Relation
compositionOfBinaryRelations = undefined

isFuzzyEquivalence :: Relation -> Bool
isFuzzyEquivalence = undefined

extractDomainPairs :: Relation -> [(DomainElement, DomainElement)]
extractDomainPairs r = let
    d = domain r
    c = getComponent d 0
    in [(p1, p2) | p1 <- iterator c, p2 <- iterator c]
