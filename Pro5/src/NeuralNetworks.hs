module NeuralNetworks where

import           Control.Arrow
import           Control.Monad
import           Data.Matrix
import           Data.Vector   (Vector)
import qualified Data.Vector   as V
import           Utility

data NN where
    NN :: { nnLayers :: Vector NNLayer } -> NN

instance Show NN where
    show NN { .. } = unlines $ "NN:" : map (("  " ++) . show) (V.toList nnLayers)

scalarProduct :: (Num a) => Vector a -> Vector a -> a
scalarProduct = curry $ (! (1, 1)) . uncurry (*) . (rowVector *** colVector)

nn :: [Int] -> IO NN
nn [] = error "NN: number of layers must be greater than zero."
nn layers = do
    otherLayers <- do
        let layersP = layers ++ [1]
            layers3 = (layersP `zip3` tail layersP) $ tail (tail layersP)
        (V.fromList <$>) $ forM layers3 $ \(fanIn, current, fanOut) -> do
            nnNodes <- V.replicateM current $ do
                let stretching = 2 / fromIntegral (fanIn + fanOut)
                nnWeights <- V.replicateM fanIn ((* stretching) <$> randomPM1)
                let nnBias = 0
                return NNNode { .. }
            let nnActivationFunction = SigmoidFunction
            return NNLayer { .. }
    let nnLayers = otherLayers
    return NN { .. }

data NNLayer where
    NNLayer :: { nnNodes :: Vector NNNode, nnActivationFunction :: ActivationFunction } -> NNLayer

instance Show NNLayer where
    show NNLayer { .. } = unlines $
        ("NNLayer: ActivationFunction = " ++ show nnActivationFunction) : map (("    " ++ ) . show) (V.toList nnNodes)

data ActivationFunction = SigmoidFunction | IdentityFunction
    deriving Show

class DerivableFunction a where
    function   :: a -> BinaryFunction
    derivative :: a -> BinaryFunction

instance DerivableFunction ActivationFunction where
    function SigmoidFunction  = (1 /) . (1 +) . exp . negate
    function IdentityFunction = id

    derivative SigmoidFunction  = let
        f = function SigmoidFunction
        in uncurry (*) . (f &&& ((1 -) . f))
    derivative IdentityFunction = const 1

type BinaryFunction = Double -> Double

data NNNode where
    NNNode :: { nnWeights :: Vector Double, nnBias :: Double } -> NNNode
    deriving Show

identityNode :: Int -> NNNode
identityNode n = let
    nnWeights = V.replicate n 1
    nnBias = 0
    in NNNode { .. }

layerForwardPass :: NNLayer -> Vector Double -> Vector Double
layerForwardPass NNLayer { .. } input = V.map (\ NNNode { .. } -> nnWeights `scalarProduct` input) nnNodes

forwardPass :: NN -> Vector Double -> Vector Double
forwardPass NN { .. } input = foldl (flip layerForwardPass) input nnLayers

forwardScanl :: NN -> Vector Double -> Vector (Vector Double)
forwardScanl NN { .. } input = V.scanl (flip layerForwardPass) input nnLayers

forwardPasses :: NN -> Matrix Double -> Matrix Double
forwardPasses NN { .. } inputs = undefined

backprop :: Vector Double -> Vector Double -> NN -> NN
backprop output expected nn1@NN { .. } = let
    nnLayers = undefined
    in undefined
