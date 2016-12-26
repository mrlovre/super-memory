
module NeuralNetworks where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.State
import           Data.Function
import           Data.Matrix         (Matrix)
import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as V
import           Utility

data NN where
    NN :: { nnLayers :: Vector NNLayer } -> NN

instance Show NN where
    show NN { .. } = unlines $ "NN:" : map (("  " ++) . show) (V.toList nnLayers)

dot :: (Num a) => Vector a -> Vector a -> a
x1 `dot` x2 = V.sum $ V.zipWith (*) x1 x2

add :: (Num a) => Vector a -> Vector a -> Vector a
add = V.zipWith (+)

sub :: (Num a) => Vector a -> Vector a -> Vector a
sub = V.zipWith (-)

nn :: [Int] -> IO NN
nn [] = error "NN: number of layers must be greater than zero."
nn layers = do
    nnLayers <- do
        let layersP = layers ++ [1]
            layers3 = zip3 layersP (tail layersP) (tail $ tail layersP)
        (V.fromList <$>) $ forM layers3 $ \(fanIn, current, fanOut) -> do
            nnNodes <- V.replicateM current $ do
                let stretching = 2 / fromIntegral (fanIn + fanOut)
                nnWeights <- V.replicateM fanIn ((* stretching) <$> randomPM1)
                let nnBias = 0
                return NNNode { .. }
            return NNLayer { .. }
    return NN { .. }

data NNLayer where
    NNLayer :: { nnNodes :: Vector NNNode } -> NNLayer

instance Show NNLayer where
    show NNLayer { .. } = unlines $ map (("    " ++ ) . show) (V.toList nnNodes)

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
layerForwardPass NNLayer { .. } input = V.map (\ NNNode { .. } -> nnWeights `dot` input) nnNodes

forwardPass :: NN -> Vector Double -> Vector Double
forwardPass NN { .. } input = foldl (flip layerForwardPass) input nnLayers

forwardScanl :: NN -> Vector Double -> Vector (Vector Double)
forwardScanl NN { .. } input = V.scanl (flip layerForwardPass) input nnLayers

forwardPasses :: NN -> Matrix Double -> Matrix Double
forwardPasses NN { .. } inputs = undefined

backpropGradient :: NN -> Vector Double -> Vector Double -> NN
backpropGradient neuralNetwork input expected = let
    outputs = forwardScanl neuralNetwork input
    layers = nnLayers neuralNetwork
    nLayers = V.length layers
    outputDifference = expected `sub` V.last outputs
    errors = let
        processLayer rI layer = let
            i = nLayers - rI - 1
            processNodeLastLayer j NNNode { .. } = let
                der = derivative SigmoidFunction
                in der (outputs ! succ i ! j) * (outputDifference ! j)
            processNode j NNNode { .. } = do
                prevErr <- get
                let der = derivative SigmoidFunction
                return $ der (outputs ! succ i ! j) * (prevErr `dot` nnWeights)
            in if
                | i == pred nLayers -> do
                    let err = V.imap processNodeLastLayer (nnNodes layer)
                    put err
                    return err
                | otherwise -> do
                    err <- V.imapM processNode (nnNodes layer)
                    put err
                    return err
        in V.reverse $ V.imapM processLayer (V.reverse layers) `evalState` V.empty
    gradients = let
        makeLayer i output = let
            err = errors ! i
            makeNode e = let
                nnWeights = V.map (e *) output
                nnBias = 0
                in NNNode { .. }
            nnNodes = V.map makeNode err
            in NNLayer { .. }
        in V.imap makeLayer $ V.init outputs
    in NN { nnLayers = gradients }

backpropCorrection :: Double -> NN -> NN -> NN
backpropCorrection eps gradients neuralNetwork = let
    subtractGradient g n = n + g * eps
    newNNLayers = V.zipWith combineLayers (nnLayers gradients) (nnLayers neuralNetwork)
    combineLayers grads layer = let
        combineNodes g n = let
            newNNWeights = V.zipWith subtractGradient (nnWeights g) (nnWeights n)
            newNNBias = nnBias g `subtractGradient` nnBias n
            in NNNode { nnWeights = newNNWeights, nnBias = newNNBias }
        newNNNodes = V.zipWith combineNodes (nnNodes grads) (nnNodes layer)
        in NNLayer { nnNodes = newNNNodes }
    in NN { nnLayers = newNNLayers }

meanGradient :: Vector NN -> NN
meanGradient grads = let
    combineGradients l r = let
        newNNLayers = (V.zipWith combineLayers `on` nnLayers) l r
        in NN { nnLayers = newNNLayers }
    combineLayers l r = let
        newNNNodes = (V.zipWith combineNodes `on` nnNodes) l r
        in NNLayer { nnNodes = newNNNodes }
    combineNodes l r = let
        newNNWeights = (V.zipWith (+) `on` nnWeights) l r
        newNNBias = ((+) `on` nnBias) l r
        in NNNode { nnWeights = newNNWeights, nnBias = newNNBias }
    in V.foldl1 combineGradients grads
