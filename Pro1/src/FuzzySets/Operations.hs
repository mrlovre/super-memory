-- | Provides various unary and binary operations.
module FuzzySets.Operations where

-- | Type of an unary function.
type UnaryFunction = Double -> Double

-- | Type of a binary function.
type BinaryFunction = Double -> Double -> Double

-- | Zadeh complement.
zadehNot :: Double -> Double
zadehNot = (1 -)

-- | Zadeh intersection.
zadehAnd :: Double -> Double -> Double
zadehAnd = min

-- | Zadeh union.
zadehOr :: Double -> Double -> Double
zadehOr = max

-- | Hamacher parametrised union.
hamacherSumParam :: Double -> Double -> Double -> Double
hamacherSumParam nu a b = if
    | nu < 0 -> error "Parameter must be non-negative."
    | otherwise -> (a + b - (2 - nu) * a * b) / (1 - (1 - nu) * a * b)

-- | Hamacher parametrised intersection.
hamacherProductParam :: Double -> Double -> Double -> Double
hamacherProductParam nu a b = if
    | nu < 0 -> error "Parameter must be non-negative."
    | otherwise -> a * b / (nu + (1 - nu) * (a + b - a * b))

-- | Hamacher union.
hamacherSum :: Double -> Double -> Double
hamacherSum = hamacherSumParam 0

-- | Hamacher intersection.
hamacherProduct :: Double -> Double -> Double
hamacherProduct = hamacherProductParam 0

-- | Yager union.
yagerSumParam :: Double -> Double -> Double -> Double
yagerSumParam q a b = if
    | q < 0 -> error "Parameter must be non-negative."
    | otherwise -> 1 - min 1 ((1 - a) ** q + (1 - b) ** q) ** (1 / q)

-- | Yager intersection.
yagerProductParam :: Double -> Double -> Double -> Double
yagerProductParam q a b = if
    | q < 0 -> error "Parameter must be non-negative."
    | otherwise -> min 1 (a ** q +  b ** q) ** (1 / q)

-- | Frank union.
frankSumParam :: Double -> Double -> Double -> Double
frankSumParam s a b = if
    | s <= 0 || s == 1 -> error "Parameter must be positive, and not equal to 1."
    | otherwise -> logBase s (1 + (s ** a - 1) * (s ** b - 1) / (s - 1))

-- | Frank intersection.
frankProductParam :: Double -> Double -> Double -> Double
frankProductParam s a b = if
    | s <= 0 || s == 1 -> error "Parameter must be positive, and not equal to 1."
    | otherwise -> 1 - logBase s (1 + (s ** (1 - a) - 1) * (s ** (1 - b) - 1) / (s - 1))

-- | Dubois-Prade union.
duboisPradeSumParam :: Double -> Double -> Double -> Double
duboisPradeSumParam alpha a b = if
    | alpha < 0 || alpha > 1 -> error "Parameter must be in range [0, 1]."
    | otherwise -> a * b / max alpha (max a b)

-- | Dubois-Prade intersection.
duboisPradeProductParam :: Double -> Double -> Double -> Double
duboisPradeProductParam alpha a b = if
    | alpha < 0 || alpha > 1 -> error "Parameter must be in range [0, 1]."
    | otherwise -> (a + b - a * b - min (1 - alpha) (min a b)) / max alpha (max (1 - a) (1 - b))

-- | Sugen parametrised complement.
sugenNotParam :: Double -> Double -> Double
sugenNotParam alpha a = (1 - a) / (1 + alpha * a)

-- | Yager parametrised complement.
yagerNotParam :: Double -> Double -> Double
yagerNotParam beta a = (1 + a** beta) ** (1 / beta)

-- | Algebraic union.
algebraicSum :: Double -> Double -> Double
algebraicSum = hamacherSumParam 1

-- | Algebraic intersection.
algebraicProduct :: Double -> Double -> Double
algebraicProduct = duboisPradeProductParam 1

-- | Einstein union.
einsteinSum :: Double -> Double -> Double
einsteinSum = hamacherSumParam 2

-- | Einstein intersection.
einsteinProduct :: Double -> Double -> Double
einsteinProduct = hamacherProductParam 2

-- | Limited union.
limitedSum :: Double -> Double -> Double
limitedSum = yagerSumParam 1

-- | Limited intersection.
limitedProduct :: Double -> Double -> Double
limitedProduct = yagerProductParam 1

-- | Drasticu union.
drasticSum :: Double -> Double -> Double
drasticSum a b = if
    | max a b == 1 -> min a b
    | otherwise -> 0

-- | Drastic intersection.
drasticProduct :: Double -> Double -> Double
drasticProduct a b = if
    | min a b == 0 -> max a b
    | otherwise -> 0
