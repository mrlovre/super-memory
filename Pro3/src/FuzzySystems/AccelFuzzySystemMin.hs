module FuzzySystems.AccelFuzzySystemMin where

import           Defuzzifiers.Defuzzifier
import           Domains.Domain
import           FuzzySystems.FuzzySystem
import           LinguisticVariables.AccelLinguisticVariables
import           LinguisticVariables.DistanceLinguisticVariables
import           LinguisticVariables.LinguisticVariableHelper
import           LinguisticVariables.SpeedLinguisticVariables
import           Rules.Rule

data AccelFuzzySystemMin where
    AccelFuzzySystemMin :: ADefuzzifier -> ADomain -> Ruleset -> AccelFuzzySystemMin

instance FuzzySystem AccelFuzzySystemMin where
    getRuleset (AccelFuzzySystemMin _ _ r) = r
    getDefuzzifier (AccelFuzzySystemMin d _ _) = d
    getDomain (AccelFuzzySystemMin _ d _) = d

accelFuzzySystemMin :: ADefuzzifier -> AccelFuzzySystemMin
accelFuzzySystemMin defuzzifier = AccelFuzzySystemMin defuzzifier acceleration accelRuleset

accelRuleset :: Ruleset
accelRuleset = [accelRule1, accelRule2, accelRule3, accelRule4, accelRule5, accelRule6]

accelRule1 :: Rule
accelRule1 vars = let
    [l, d, lk, dk, v] = getVariables vars ["l", "d", "lk", "dk", "v"]
    in makeRule [(l, farDistance), (d, farDistance), (lk, farDistance), (dk, farDistance), (v, lvNot fastSpeed)] weakAccel

accelRule2 :: Rule
accelRule2 vars = let
     [l, lk, v] = getVariables vars ["l", "lk", "v"]
     in makeRule [(l, closeDistance), (lk, closeDistance), (v, fastSpeed)] brake

accelRule3 :: Rule
accelRule3 vars = let
    [d, dk, v] = getVariables vars ["d", "dk", "v"]
    in makeRule [(d, closeDistance), (dk, closeDistance), (v, fastSpeed)] brake

accelRule4 :: Rule
accelRule4 vars = let
    [l, lk, v] = getVariables vars ["l", "lk", "v"]
    in makeRule [(l, criticallyCloseDistance), (lk, lvNot criticallyCloseDistance), (v, lvNot topSpeed)] topAccel

accelRule5 :: Rule
accelRule5 vars = let
    [d, dk, v] = getVariables vars ["d", "dk", "v"]
    in makeRule [(d, criticallyCloseDistance), (dk, lvNot criticallyCloseDistance), (v, lvNot topSpeed)] topAccel

accelRule6 :: Rule
accelRule6 vars = let
    [v] = getVariables vars ["v"]
    in makeRule [(v, fastSpeed)] neutralAccel
