module Process where

import           Control.Arrow
import           Control.Monad

import           Graphics.UI.Gtk
import           Variables
import           Utility

decimationFactor :: Int
decimationFactor = 10

normalizeData :: [Point] -> [DecimalPoint]
normalizeData points = let
    (m, n) = join (***) fromIntegral . (maximum &&& minimum) $ concatMap flattenTuple points
    flattenTuple (a, b) = [a, b]
    (meanX, meanY) = join (***) (/ fromIntegral (length points)) . join (***) fromIntegral $
            foldl (\ (a, b) (c, d) -> (a + c, b + d)) (0, 0) points
    in map (\ (x, y) -> ((fromIntegral x - meanX) / (m - n), (fromIntegral y - meanY) / (m - n))) points

decimateData :: [Point] -> [Point]
decimateData [] = []
decimateData points = let
    segmentLength = length points `div` pred decimationFactor
    in [points !! (i * segmentLength) | i <- [0 .. decimationFactor - 2]] ++ [last points]

data NN where
    NN :: { layers :: [NNLayer] } -> NN

nn :: [Int] -> IO NN
nn [] = error "NN: number of layers must be greater than zero."
nn l@(f : _) = do
    let firstLayer = let
            nodes = replicate f identityNode
            activationFunction = IdentityFunction
            in NNLayer { .. }
    otherLayers <- do
        let layersP = l ++ [1]
            layers3 = (layersP `zip3` tail layersP) $ tail (tail layersP)
        forM layers3 $ \(fanIn, current, fanOut) -> do
            nodes <- replicateM current $ do
                let stretching = 2 / fromIntegral (fanIn + fanOut)
                weights <- replicateM fanIn ((* stretching) <$> randomPM1)
                let bias = 0
                return NNNode { .. }
            let activationFunction = SigmoidFunction
            return NNLayer { .. }
    let layers = firstLayer : otherLayers
    return NN { .. }

data NNLayer where
    NNLayer :: { nodes :: [NNNode], activationFunction :: ActivationFunction } -> NNLayer

data ActivationFunction = SigmoidFunction | IdentityFunction

class DerivableFunction a where
    function   :: a -> BinaryFunction
    derivative :: a -> BinaryFunction

instance DerivableFunction ActivationFunction where
    function SigmoidFunction  = (1 /) . (1 +) . exp . negate
    function IdentityFunction = id

    derivative SigmoidFunction = let
        f = function SigmoidFunction
        in uncurry (*) . (f &&& ((1 -) . f))
    derivative IdentityFunction = const 1

type BinaryFunction = Double -> Double

data NNNode where
    NNNode :: { weights :: [Double], bias :: Double } -> NNNode

identityNode :: NNNode
identityNode = let
    weights = [1]
    bias = 0
    in NNNode { .. }
