module FuzzySets.FuzzySetHelper where

import FuzzySets.FuzzySet
import FuzzySets.CalculatedFuzzySet
import Domains.Domain
import Domains.Dimensionable

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

unaryOperation :: AFuzzySet -> (Double -> Double) -> AFuzzySet
unaryOperation fuzzySet f = let
    d = domain fuzzySet
    in AFuzzySet $ calculatedFuzzySet (f . valueAt fuzzySet . elementAtIndex d) d

binaryOperation :: AFuzzySet -> AFuzzySet -> (Double -> Double -> Double) -> AFuzzySet
binaryOperation a b f = let
    [da, db] = map domain [a, b]
    valueAtIndex s = valueAt s . elementAtIndex da
    in if
        | dimension da /= dimension db || cardinality da /= cardinality db ->
            error "Incompatible domains for binary operation."
        | otherwise -> AFuzzySet $ calculatedFuzzySet (\ i -> f (valueAtIndex a i) (valueAtIndex b i)) da
