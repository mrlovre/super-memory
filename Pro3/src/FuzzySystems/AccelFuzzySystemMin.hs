module FuzzySystems.AccelFuzzySystemMin where

import           Configuration
import           Defuzzifiers.Defuzzifier
import           Domains.Domain
import           FuzzySets.FuzzySetHelper
import           FuzzySets.Operations
import           FuzzySystems.FuzzySystem
import           FuzzySystems.FuzzySystemHelper
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
accelRuleset = [accelRule1]

accelRule1 :: Rule
accelRule1 vars = let
    [lk, dk, v] = map (getVariable vars) ["lk", "dk", "v"]
    in makeRule [(lk, farDistance), (dk, farDistance), (v, lvNot fastSpeed)] weakAccel
