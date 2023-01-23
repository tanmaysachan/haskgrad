module Autodiff where

import qualified Data.Map as Map

-- Function context to be used in backprop
data Context a = Context {grad :: Bool, savedValues :: [a]}

localDerivative :: ([Float] -> Float) -> [Float] -> Int -> Float
localDerivative f xs i = (f1 - (f xs)) / eps
                where f1 = f (take i xs ++ (xs !! i + eps) : drop (i+1) xs)
                      eps = 0.00001