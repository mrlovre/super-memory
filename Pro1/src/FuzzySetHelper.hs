module FuzzySetHelper where

lFunctionGenerator :: Int -> Int -> Int -> Double
lFunctionGenerator a b x = if
    | a > b -> error "Function is not well defined."
    | x <= a -> 1
    | x < b -> fromIntegral (b - x) / fromIntegral (b - a)
    | otherwise -> 0

gammaFunctionGenerator :: Int -> Int -> Int -> Double
gammaFunctionGenerator a b x = if
    | a > b -> error "Function is not well defined."
    | x <= a -> 0
    | x < b -> fromIntegral (x - a) / fromIntegral (b - a)
    | otherwise -> 1

lambdaFunctionGenerator :: Int -> Int -> Int -> Int -> Double
lambdaFunctionGenerator a b c x = if
    | a > b || b > c -> error "Function is not well defined."
    | x <= a -> 0
    | x < b -> fromIntegral (x - a) / fromIntegral (b - a)
    | x == b -> 1
    | x < c -> fromIntegral (c - x) / fromIntegral (c - b)
    | otherwise -> 0
