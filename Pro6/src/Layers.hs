module Layers where

import           Data.Vector (Vector)
import qualified Data.Vector as V


data MembershipFunction where
    Sigmoid :: { mfa :: !Double, mfb :: !Double } -> MembershipFunction

deriving instance Show MembershipFunction

evaluateMembership :: MembershipFunction -> Double -> Double
evaluateMembership Sigmoid {..} x = 1.0 / (1.0 + exp (-mfb * (x - mfa)))

membershipParameterDerivatives :: MembershipFunction -> Double -> [Double]
membershipParameterDerivatives sigmoid@Sigmoid {..} x = let
    s = evaluateMembership sigmoid x
    ds = s * (1.0 - s)
    derA = ds * (-mfb)
    derB = ds * (x - mfa)
    in [derA, derB]


data TNorm where
    AlgebraicProduct :: TNorm

deriving instance Show TNorm

evaluateTNorm :: TNorm -> Double -> Double -> Double
evaluateTNorm AlgebraicProduct = (*)

tNormDerivativeX :: TNorm -> Double -> Double -> Double
tNormDerivativeX AlgebraicProduct _ y = y

tNormDerivativeY :: TNorm -> Double -> Double -> Double
tNormDerivativeY AlgebraicProduct x _ = x


normalizeWeights :: Vector Double -> Vector Double
normalizeWeights weights = let
    totalWeights = V.sum weights
    in if totalWeights /= 0.0
        then V.map (/ totalWeights) weights
        else V.map (const $ 1.0 / fromIntegral (length weights)) weights


data TransferFunction where
    LinearCombination :: { tfp :: !Double, tfq :: !Double, tfr :: !Double } -> TransferFunction

deriving instance Show TransferFunction

evaluateTransfer :: TransferFunction -> [Double] -> Double
evaluateTransfer LinearCombination {..} [f1, f2] = tfp * f1 + tfq * f2 + tfr
evaluateTransfer LinearCombination {} _ = error "Linear combination expects two values, f1 and f2."

transferParameterDerivatives :: TransferFunction -> [Double] -> [Double]
transferParameterDerivatives LinearCombination {..} [f1, f2] = let
    derP = f1
    derQ = f2
    derR = 1.0
    in [derP, derQ, derR]
transferParameterDerivatives LinearCombination {} _ = error "Linear combination expects two values, f1 and f2."
