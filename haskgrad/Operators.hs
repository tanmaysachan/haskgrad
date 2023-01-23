module Operators where

isClose :: Float -> Float -> Bool
isClose x y = (<=0.01) (abs (x-y))

-- stable sigmoid
sigmoid :: Float -> Float
sigmoid x
    | x >= 0.0 = 1.0/(1.0 + (exp . negate $ x))
    | otherwise = (exp x)/(1 + (exp x))

relu :: Float -> Float
relu x = maximum [x, 0.0]