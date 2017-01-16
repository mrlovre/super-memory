module Main where

import           Data.Function
import           Data.List
import           Data.Ord
import           Data.Vector                            (Vector, (!))
import qualified Data.Vector                            as V
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           NN

main :: IO ()
main = do
    toFile def "function.png" $ do
        layout_title .= "Function"
        setColors [opaque blue, opaque red, opaque green, opaque yellow, opaque cyan]
        let y w s x = 1 / (1 + abs ((x - w) / s))
            xs = [-8, -7.9 .. 12] :: [Double]
            ss = [0.25, 1, 4] :: [Double]
            funs = map (\ s' -> [xs `zip` map (y 2 s') xs]) ss
        mapM_ (plot . uncurry line) (map (("s=" ++) . show) ss `zip` funs)
    dataset <- map (map read . words) . lines <$> readFile "dataset.txt" :: IO [[Double]]
    toFile def "clusters.png" $ do
        layout_title .= "Clusters"
        setColors [opaque blue, opaque red, opaque green, opaque yellow, opaque cyan]
        let indClasses = map show [1 ..] `zip` classes
            classes = divideClasses dataset
        mapM_ (plot . uncurry points) indClasses
    trained <- read <$> readFile "train.model" :: IO NN
    toFile def "trained.png" $ do
        layout_title .= "Trained NN"
        setColors [opaque blue, opaque red, opaque green, opaque yellow, opaque cyan]
        let indClasses = map show [1 ..] `zip` classes
            classes = divideClasses dataset
            centroids = map (\ v -> (v ! 0, v ! 1)) $ V.toList $ t1ws $ V.head $ nnLayers trained
        mapM_ (plot . uncurry points) indClasses
        plot $ points "learned centroids" centroids
    trained2 <- read <$> readFile "train_2_8_4_3.model" :: IO NN
    toFile def "trained2.png" $ do
        layout_title .= "Trained NN"
        setColors [opaque blue, opaque red, opaque green, opaque yellow, opaque cyan]
        let indClasses = map show [1 ..] `zip` classes
            classes = divideClasses dataset
            centroids = map (\ v -> (v ! 0, v ! 1)) $ V.toList $ t1ws $ V.head $ nnLayers trained2
        mapM_ (plot . uncurry points) indClasses
        plot $ points "learned centroids" centroids
    trained3 <- read <$> readFile "train_2_6_4_3.model" :: IO NN
    toFile def "trained3.png" $ do
        layout_title .= "Trained NN"
        setColors [opaque blue, opaque red, opaque green, opaque yellow, opaque cyan]
        let indClasses = map show [1 ..] `zip` classes
            classes = divideClasses dataset
            centroids = map (\ v -> (v ! 0, v ! 1)) $ V.toList $ t1ws $ V.head $ nnLayers trained3
        mapM_ (plot . uncurry points) indClasses
        plot $ points "learned centroids" centroids

divideClasses :: [[Double]] -> [[(Double, Double)]]
divideClasses dataset = let
    list `indexOf` e = let
        ~(Just (i, _)) = ((== e) . snd) `find` ([1 ..] `zip` list)
        in i
    go (x : y : oneHot) = ((x, y), oneHot `indexOf` 1)
    in map (map fst) $ groupBy ((==) `on` snd) $ sortBy (comparing snd) $ map go dataset
