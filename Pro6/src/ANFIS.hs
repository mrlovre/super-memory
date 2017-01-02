module ANFIS where

import           Control.Arrow
import           Control.Monad
import           Data.Tuple.Curry
import           Data.Vector            (Vector, (!))
import qualified Data.Vector            as V

import           Layers
import           Sample
import           Utility


type MembershipLayer = Vector (Vector MembershipFunction)
type TransferLayer = Vector TransferFunction


data ANFIS where
    ANFIS :: { membershipLayer :: MembershipLayer, tNorm :: TNorm, transferLayer :: TransferLayer, m :: Int } -> ANFIS

deriving instance Show ANFIS

data TrainingAlgorithm = StochasticGradientDescent | GradientDescent


anfisInitialize :: Int -> Int -> IO ANFIS
anfisInitialize d m = do
    let randomMembershipFunction = do
            [a, b] <- replicateM 2 randomDouble
            return $ Sigmoid a b
    [ps, qs, rs] <- replicateM 3 $ replicateM m randomDouble
    membershipLayer <- V.replicateM d $ V.replicateM m randomMembershipFunction
    let tNorm = AlgebraicProduct
        transferLayer = V.fromList $ map (uncurryN LinearCombination) $ zip3 ps qs rs
    return ANFIS { .. }

trainANFIS :: [(Sample, Output)] -> TrainingAlgorithm -> Double -> ANFIS -> ANFIS
trainANFIS samples algorithm eta anfis = let
    sampleGradients anfisCurrent@ANFIS { .. } (sample@Sample { .. }, expected) = let
        (membershipLayerOutputs, tNormLayerNormalizedOutputs, transferLayerOutputs) = runANFISLayers anfisCurrent sample
        output = runANFIS anfisCurrent sample
        derErrOut = output - expected
        derOutWeight i = transferLayerOutputs ! i
        derWeightA i = snd membershipLayerOutputs ! i
        derWeightB i = fst membershipLayerOutputs ! i
        derAMembershipParams i = membershipParameterDerivatives $ membershipLayer ! 0 ! i
        derBMembershipParams i = membershipParameterDerivatives $ membershipLayer ! 1 ! i
        gradMembershipParams = let
            gradForARule i = map (derErrOut * derOutWeight i * derWeightA i *) $ derAMembershipParams i sx
            gradForBRule i = map (derErrOut * derOutWeight i * derWeightB i *) $ derBMembershipParams i sy
            in V.fromList [V.fromList $ map gradForRule [0 .. pred m] | gradForRule <- [gradForARule, gradForBRule]]
        derOutTransfer i = tNormLayerNormalizedOutputs ! i
        derTransferParams i = transferParameterDerivatives $ transferLayer ! i
        gradTransferParams = let
            gradForRule i = map (derErrOut * derOutTransfer i *) $ derTransferParams i [sx, sy]
            in V.fromList $ map gradForRule [0 .. pred m]
        in (gradMembershipParams, gradTransferParams)
    updateTransferFunction LinearCombination { .. } grads = let
        [nTfp, nTfq, nTfr] = zipWith (-) [tfp, tfq, tfr] $ map (eta *) grads
        in LinearCombination nTfp nTfq nTfr
    updateMembershipLayer vectorTransferFunctions vectorGrads = let
        updateMembershipFunction Sigmoid { .. } grads = let
            [nMfa, nMfb] = zipWith (-) [mfa, mfb] $ map (eta *) grads
            in Sigmoid nMfa nMfb
        in V.zipWith updateMembershipFunction vectorTransferFunctions vectorGrads
    in case algorithm of
        StochasticGradientDescent -> let
            stochasticUpdate anfisCurrent@ANFIS { .. } sampleOutput = let
                (gradMembershipParams, gradTransferParams) = sampleGradients anfisCurrent sampleOutput
                newMembershipLayer = V.zipWith updateMembershipLayer membershipLayer gradMembershipParams
                newTransferLayer = V.zipWith updateTransferFunction transferLayer gradTransferParams
                in anfisCurrent { membershipLayer = newMembershipLayer, transferLayer = newTransferLayer }
            in foldl stochasticUpdate anfis samples
        GradientDescent -> let
            update ANFIS { .. } (gradMembershipParams, gradTransferParams) = let
                newMembershipLayer = V.zipWith updateMembershipLayer membershipLayer gradMembershipParams
                newTransferLayer = V.zipWith updateTransferFunction transferLayer gradTransferParams
                in anfis { membershipLayer = newMembershipLayer, transferLayer = newTransferLayer }
            grads = map (sampleGradients anfis) samples
            in foldl update anfis grads

runANFIS :: ANFIS -> Sample -> Output
runANFIS a sample = let
    (_, tNormLayerNormalizedOutputs, transferLayerOutputs) = runANFISLayers a sample
    in V.sum $ V.zipWith (*) tNormLayerNormalizedOutputs transferLayerOutputs

runANFISLayers :: ANFIS -> Sample -> ((Vector Double, Vector Double), Vector Double, Vector Double)
runANFISLayers ANFIS { .. } Sample { .. } = let
    membershipLayerOutputs = let
        evaluateMembershipWith a = V.map (`evaluateMembership` a)
        in (evaluateMembershipWith sx *** evaluateMembershipWith sy) $ ((! 0) &&& (! 1)) membershipLayer
    tNormLayerOutputs = (uncurry $ V.zipWith (evaluateTNorm tNorm)) membershipLayerOutputs
    tNormLayerNormalizedOutputs = normalizeWeights tNormLayerOutputs
    transferLayerOutputs = V.map (`evaluateTransfer` [sx, sy]) transferLayer
    in (membershipLayerOutputs, tNormLayerNormalizedOutputs, transferLayerOutputs)
