module FuzzySystems.RudderFuzzySystemMin where

import           Defuzzifiers.Defuzzifier
import           Domains.Domain
import           FuzzySystems.FuzzySystem
import           LinguisticVariables.AngleLinguisticVariables
import           LinguisticVariables.DistanceLinguisticVariables
import           LinguisticVariables.LinguisticVariableHelper
import           Rules.Rule

data RudderFuzzySystemMin where
    RudderFuzzySystemMin :: ADefuzzifier -> ADomain -> Ruleset -> RudderFuzzySystemMin

instance FuzzySystem RudderFuzzySystemMin where
    getRuleset (RudderFuzzySystemMin _ _ r) = r
    getDefuzzifier (RudderFuzzySystemMin d _ _) = d
    getDomain (RudderFuzzySystemMin _ d _) = d

rudderFuzzySystemMin :: ADefuzzifier -> RudderFuzzySystemMin
rudderFuzzySystemMin defuzzifier = RudderFuzzySystemMin defuzzifier angle rudderRuleset

rudderRuleset :: Ruleset
rudderRuleset = [rudderRule1a, rudderRule1b, rudderRule2a, rudderRule2b, rudderRule5]

rudderRule1a :: Rule
rudderRule1a vars = let
    [lk, d, dk] = getVariables vars ["lk", "d", "dk"]
    in makeRule [(lk, closeDistance), (d, lvNot criticallyCloseDistance), (dk, lvNot closeDistance) ] $ lvVery sharpRightAngle

rudderRule1b :: Rule
rudderRule1b vars = let
    [l, d, dk] = getVariables vars ["l", "d", "dk"]
    in makeRule [(l, criticallyCloseDistance), (d, lvNot criticallyCloseDistance), (dk, lvNot closeDistance)] $ lvVery sharpRightAngle

rudderRule2a :: Rule
rudderRule2a vars = let
    [dk, l, lk] = getVariables vars ["dk", "l", "lk"]
    in makeRule [(dk, closeDistance), (l, lvNot criticallyCloseDistance), (lk, lvNot closeDistance)] $ lvVery sharpLeftAngle

rudderRule2b :: Rule
rudderRule2b vars = let
    [d, l, lk] = getVariables vars ["d", "l", "lk"]
    in makeRule [(d, criticallyCloseDistance), (l, lvNot criticallyCloseDistance), (lk, lvNot closeDistance)] $ lvVery sharpLeftAngle

rudderRule3 :: Rule
rudderRule3 vars = let
    [l, lk] = getVariables vars ["l", "lk"]
    in makeRule [(l, closeDistance), (lk, closeDistance)] $ lvVery rightAngle

rudderRule4 :: Rule
rudderRule4 vars = let
    [d, dk] = getVariables vars ["d", "dk"]
    in makeRule [(d, closeDistance), (dk, closeDistance)] $ lvVery leftAngle

rudderRule5 :: Rule
rudderRule5 vars = let
    [l, lk, d, dk] = getVariables vars ["l", "lk", "d", "dk"]
    in makeRule [(l, farDistance), (lk, farDistance), (d, farDistance), (dk, farDistance)] neutralAngle
