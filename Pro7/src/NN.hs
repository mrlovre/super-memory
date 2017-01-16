module NN where

import           Control.Monad.Random
import           Data.Vector          (Vector, (!))
import qualified Data.Vector          as V

import           Utility

data NN where
    NN :: {nnLayers :: !(Vector NNLayer)} -> NN
    deriving (Show, Read)

nn :: [Int] -> IO NN
nn layers = do
    firstLayer <- case layers of
        f : s : _ -> nnLayerT1 f s
        _         -> error "Error: not enough layers."
    restLayers <- case layers of
        (_ : rest) -> zipWithM nnLayerT2 rest (tail rest)
        _          -> error "Error: not enough layers."
    let nnLayers = V.fromList (firstLayer : restLayers)
    return NN{..}

nnForwardPass :: NN -> Vector Double -> Vector Double
nnForwardPass NN{..} input = V.foldl (flip nnLayerForwardPass) input nnLayers

data NNLayer where
    NNLayerT1 :: {t1ws :: !(Vector (Vector Double)), ss :: !(Vector (Vector Double)), t1N :: !Int} -> NNLayer
    NNLayerT2 :: {t2ws :: !(Vector (Vector Double)), bs :: !(Vector Double), t2N :: !Int} -> NNLayer
    deriving (Show, Read)

nnLayerT1 :: Int -> Int -> IO NNLayer
nnLayerT1 input n = do
    [t1ws, ss] <- replicateM 2 $ V.replicateM n $ V.fromListN input <$> getGausses 0.1
    let t1N = n
    return NNLayerT1{..}

t1TF :: Vector Double -> Vector Double -> Vector Double -> Double
t1TF ws ss xs = 1 / (1 + V.sum (V.zipWith3 (\ w s x -> abs ((x - w) / s)) ws ss xs))

nnLayerT2 :: Int -> Int -> IO NNLayer
nnLayerT2 input n = do
    t2ws <- V.replicateM n $ V.fromListN input <$> getGausses 0.1
    bs <- V.fromListN n <$> getGausses 0.1
    let t2N = n
    return NNLayerT2{..}

t2TF :: Vector Double -> Double -> Vector Double -> Double
t2TF ws b xs = sigmoid $ ws `dot` xs + b

nnLayerForwardPass :: NNLayer -> Vector Double -> Vector Double
nnLayerForwardPass nnLayer input = case nnLayer of
    NNLayerT1{..} -> V.fromList $ map (\ i -> t1TF (t1ws ! i) (ss ! i) input) [0 .. pred t1N]
    NNLayerT2{..} -> V.fromList $ map (\ i -> t2TF (t2ws ! i) (bs ! i) input) [0 .. pred t2N]
