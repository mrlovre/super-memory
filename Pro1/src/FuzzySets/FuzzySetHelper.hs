-- | Fuzzy set helper functions.
module FuzzySets.FuzzySetHelper where

import FuzzySets.FuzzySet
import FuzzySets.CalculatedFuzzySet
import Domains.Domain
import Domains.Dimensionable
import FuzzySets.Operations

-- | Generates the L-function. Takes alpha and beta parameters.
lFunctionGenerator :: Int -> Int -> MembershipFunction
lFunctionGenerator a b x = if
    | a > b -> error "Function is not well defined."
    | x <= a -> 1
    | x < b -> fromIntegral (b - x) / fromIntegral (b - a)
    | otherwise -> 0

-- | Generates the Gamma-function. Takes alpha and beta parameters.
gammaFunctionGenerator :: Int -> Int -> MembershipFunction
gammaFunctionGenerator a b x = if
    | a > b -> error "Function is not well defined."
    | x <= a -> 0
    | x < b -> fromIntegral (x - a) / fromIntegral (b - a)
    | otherwise -> 1

-- | Generates the Lambda-function. Takes alpha, beta, and gamma parameters.
lambdaFunctionGenerator :: Int -> Int -> Int -> MembershipFunction
lambdaFunctionGenerator a b c x = if
    | a > b || b > c -> error "Function is not well defined."
    | x <= a -> 0
    | x < b -> fromIntegral (x - a) / fromIntegral (b - a)
    | x == b -> 1
    | x < c -> fromIntegral (c - x) / fromIntegral (c - b)
    | otherwise -> 0

-- | Applies an unary operation to a fuzzy set.
unaryOperation :: AFuzzySet -> UnaryFunction -> AFuzzySet
unaryOperation fuzzySet f = let
    d = domain fuzzySet
    in AFuzzySet $ calculatedFuzzySet (f . valueAt fuzzySet . elementAtIndex d) d

-- | Applies a binary operation to a fuzzy set.
binaryOperation :: AFuzzySet -> AFuzzySet -> BinaryFunction -> AFuzzySet
binaryOperation a b f = let
    [da, db] = map domain [a, b]
    valueAtIndex s = valueAt s . elementAtIndex da
    in if
        | dimension da /= dimension db || cardinality da /= cardinality db ->
            error "Incompatible domains for binary operation."
        | otherwise -> AFuzzySet $ calculatedFuzzySet (\ i -> f (valueAtIndex a i) (valueAtIndex b i)) da
