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
rudderRuleset = [rudderRule1, rudderRule2, rudderRule3, rudderRule4, rudderRule5]

rudderRule1 :: Rule
rudderRule1 vars = let
    [l, lk] = getVariables vars ["l", "lk"]
    in makeRule [(l, criticallyCloseDistance), (lk, criticallyCloseDistance)] sharpRightAngle

rudderRule2 :: Rule
rudderRule2 vars = let
    [d, dk] = getVariables vars ["d", "dk"]
    in makeRule [(d, criticallyCloseDistance), (dk, criticallyCloseDistance)] sharpLeftAngle

rudderRule3 :: Rule
rudderRule3 vars = let
    [l, lk] = getVariables vars ["l", "lk"]
    in makeRule [(l, closeDistance), (lk, closeDistance)] sharpRightAngle

rudderRule4 :: Rule
rudderRule4 vars = let
    [d, dk] = getVariables vars ["d", "dk"]
    in makeRule [(d, closeDistance), (dk, closeDistance)] sharpLeftAngle

rudderRule5 :: Rule
rudderRule5 vars = let
    [l, lk, d, dk] = getVariables vars ["l", "lk", "d", "dk"]
    in makeRule [(l, farDistance), (lk, farDistance), (d, farDistance), (dk, farDistance)] neutralAngle
