module FuzzySetHelper where

lFunctionGenerator :: Int -> Int -> Int -> Double
lFunctionGenerator a b x = if
    | a > b -> error ""
    | x <= a -> 1
    | x < b -> fromIntegral (b - x) / fromIntegral (b - a)
    | otherwise -> 0

gammaFunctionGenerator :: Int -> Int -> Int -> Double
gammaFunctionGenerator a b x = if
    | a > b -> error ""
    | x <= a -> 0
    | x < b -> fromIntegral (x - a) / fromIntegral (b - a)
    | otherwise -> 1

deltaFunctionGenerator :: Int -> Int -> Int -> Int -> Double
deltaFunctionGenerator a b c x = undefined
