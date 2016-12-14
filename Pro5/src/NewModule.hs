module NewModule where

a :: [Int]
a = sortBy (comparing . Down) [0 .. 5]
