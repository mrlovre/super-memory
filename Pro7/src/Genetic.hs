module Genetic where

import           Control.Monad.Random
import           Data.Vector          (Vector)
import qualified Data.Vector          as V

import qualified Configuration
import           NN
import           Utility

cross1 :: NN -> NN -> IO NN
cross1 (NN nnLayers1) (NN nnLayers2) = do
    let mergeLayers l1 l2 = case (l1, l2) of
            (NNLayerT1{t1ws = ws1, ss = ss1, t1N = n}, NNLayerT1{t1ws = ws2, ss = ss2}) -> do
                chances <- V.fromListN n <$> getRandoms
                let ws3 = V.zipWith3 (\ p w1 w2 -> if p then w1 else w2) chances ws1 ws2
                    ss3 = V.zipWith3 (\ p s1 s2 -> if p then s1 else s2) chances ss1 ss2
                return $ NNLayerT1 ws3 ss3 n
            (NNLayerT2{t2ws = ws1, bs = bs1, t2N = n}, NNLayerT2{t2ws = ws2, bs = bs2}) -> do
                chances <- V.fromListN n <$> getRandoms
                let ws3 = V.zipWith3 (\ p w1 w2 -> if p then w1 else w2) chances ws1 ws2
                    bs3 = V.zipWith3 (\ p b1 b2 -> if p then b1 else b2) chances bs1 bs2
                return $ NNLayerT2 ws3 bs3 n
    nnLayers3 <- V.zipWithM mergeLayers nnLayers1 nnLayers2
    return $ NN nnLayers3

cross2 :: NN -> NN -> IO NN
cross2  (NN nnLayers1) (NN nnLayers2) = do
    let mergeLayers l1 l2 = case (l1, l2) of
            (NNLayerT1{t1ws = ws1, ss = ss1, t1N = n}, NNLayerT1{t1ws = ws2, ss = ss2}) -> do
                chances <- V.fromListN n <$> getRandoms
                let ws3 = V.zipWith3 (\ p w1 w2 -> ((1 - p) `scalarDot` w1) `addV` (p `scalarDot` w2)) chances ws1 ws2
                    ss3 = V.zipWith3 (\ p s1 s2 -> ((1 - p) `scalarDot` s1) `addV` (p `scalarDot` s2)) chances ss1 ss2
                return $ NNLayerT1 ws3 ss3 n
            (NNLayerT2{t2ws = ws1, bs = bs1, t2N = n}, NNLayerT2{t2ws = ws2, bs = bs2}) -> do
                chances <- V.fromListN n <$> getRandoms
                let ws3 = V.zipWith3 (\ p w1 w2 -> ((1 - p) `scalarDot` w1) `addV` (p `scalarDot` w2)) chances ws1 ws2
                    bs3 = V.zipWith3 (\ p b1 b2 -> (1 - p) * b1 + p * b2) chances bs1 bs2
                return $ NNLayerT2 ws3 bs3 n
    nnLayers3 <- V.zipWithM mergeLayers nnLayers1 nnLayers2
    return $ NN nnLayers3

cross3 :: NN -> NN -> IO NN
cross3  (NN nnLayers1) (NN nnLayers2) = do
    let mergeLayers l1 l2 = case (l1, l2) of
            (NNLayerT1{t1ws = ws1, ss = ss1, t1N = n}, NNLayerT1{t1ws = ws2, ss = ss2}) -> do
                chances <- V.fromListN n <$> getRandoms
                let ws3 = V.zipWith3 (\ p w1 w2 -> ((w1 `addV` w2) `scalarDiv` 2) `addV` ((2 * (p - 0.5)) `scalarDot` (w2 `subV` w1))) chances ws1 ws2
                    ss3 = V.zipWith3 (\ p s1 s2 -> ((s1 `addV` s2) `scalarDiv` 2) `addV` ((2 * (p - 0.5)) `scalarDot` (s2 `subV` s1))) chances ss1 ss2
                return $ NNLayerT1 ws3 ss3 n
            (NNLayerT2{t2ws = ws1, bs = bs1, t2N = n}, NNLayerT2{t2ws = ws2, bs = bs2}) -> do
                chances <- V.fromListN n <$> getRandoms
                let ws3 = V.zipWith3 (\ p w1 w2 -> ((w1 `addV` w2) `scalarDiv` 2) `addV` ((2 * (p - 0.5)) `scalarDot` (w2 `subV` w1))) chances ws1 ws2
                    bs3 = V.zipWith3 (\ p b1 b2 -> (b1 + b2) / 2 + 2 * (p - 0.5) * (b2 - b1)) chances bs1 bs2
                return $ NNLayerT2 ws3 bs3 n
    nnLayers3 <- V.zipWithM mergeLayers nnLayers1 nnLayers2
    return $ NN nnLayers3

mutate1 :: NN -> IO NN
mutate1 NN{..} = do
    let mutate b p = if b
            then (p +) <$> getGauss Configuration.additiveMutationMagnitude
            else return p
        mutateV b v = if b
            then ((v `addV`) . V.fromListN (V.length v)) <$> getGausses Configuration.additiveMutationMagnitude
            else return v
    nnLayersN <- V.mapM (mutateLayer mutate mutateV) nnLayers
    return $ NN nnLayersN

mutate2 :: NN -> IO NN
mutate2 NN{..} = do
    let mutate b p = if b
            then getGauss Configuration.destructiveMutationMagnitude
            else return p
        mutateV b v = if b
            then V.fromListN (V.length v) <$> getGausses Configuration.destructiveMutationMagnitude
            else return v
    nnLayersN <- V.mapM (mutateLayer mutate mutateV) nnLayers
    return $ NN nnLayersN

mutateLayer :: MonadRandom mr => (Bool -> Double -> mr Double) ->
    (Bool -> Vector Double -> mr (Vector Double)) -> NNLayer -> mr NNLayer
mutateLayer mutate mutateV l = case l of
    NNLayerT1{..} -> do
        chances <- (V.fromListN t1N . map (< Configuration.mutationRate)) <$> getRandoms
        t1wsN <- V.zipWithM mutateV chances t1ws
        ssN <- V.zipWithM mutateV chances ss
        return $ NNLayerT1 t1wsN ssN t1N
    NNLayerT2{..} -> do
        chances <- (V.fromListN t2N . map (< Configuration.mutationRate)) <$> getRandoms
        t2wsN <- V.zipWithM mutateV chances t2ws
        bsN <- V.zipWithM mutate chances bs
        return $ NNLayerT2 t2wsN bsN t2N
