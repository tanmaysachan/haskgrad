module ScalarFunction where

import Autodiff
import qualified Operators

handleArgError :: Int -> String
handleArgError x
    | x == 0 = "Argument quantity mismatch"

data Functions = Add | Log | Mul | Inv | Neg | Sigmoid | ReLU | LessThan | EqualTo | Exp deriving Show

class ScalarFunction a where
    -- 1 compulsory input to forward
    forward :: a -> Float -> Maybe [Float] -> (Context Float, Float)
    backward :: a -> Context Float -> Float -> [Float]

-- Only scalar functions alter Scalars
instance ScalarFunction Functions where
    -- Forward passes
    -- Add function
    forward Add x (Just [y]) = (Context {grad=True, savedValues=[x, y]}, x + y)
    forward Add _ _ = error $ handleArgError 0

    -- Log function
    forward Log x Nothing = (Context {grad=True, savedValues=[x]}, log x)
    forward Log _ _ = error $ handleArgError 0

    -- Mul function
    forward Mul x (Just [y]) = (Context {grad=True, savedValues=[x, y]}, x * y)
    forward Mul _ _ = error $ handleArgError 0

    -- Inv function
    forward Inv x Nothing = (Context {grad=True, savedValues=[x]}, 1.0 / x)
    forward Inv _ _ = error $ handleArgError 0

    -- Neg function
    forward Neg x Nothing = (Context {grad=True, savedValues=[x]}, -1.0 * x)
    forward Neg _ _ = error $ handleArgError 0

    -- Sigmoid function
    forward Sigmoid x Nothing = (Context {grad=True, savedValues=[x]}, Operators.sigmoid x)
    forward Sigmoid _ _ = error $ handleArgError 0

    -- ReLU function
    forward ReLU x Nothing = (Context {grad=True, savedValues=[x]}, Operators.relu x)
    forward ReLU _ _ = error $ handleArgError 0

    -- LessThan function
    forward LessThan x (Just [y]) = (Context {grad=True, savedValues=[x, y]}, if x < y then 1.0 else 0.0)
    forward LessThan _ _ = error $ handleArgError 0

    -- EqualTo Function
    forward EqualTo x (Just [y]) = (Context {grad=True, savedValues=[x, y]}, if x == y then 1.0 else 0.0)
    forward EqualTo _ _ = error $ handleArgError 0

    -- Exp function
    forward Exp x Nothing = (Context {grad=True, savedValues=[x]}, exp x)
    forward Exp _ _ = error $ handleArgError 0

    -- Backward passes
    -- Add function
    backward Add _ x = [x, x]

    -- Log function
    backward Log (Context {savedValues=xs}) d = [d / (xs !! 0)]

    -- Mul function
    backward Mul (Context {savedValues=xs}) d = [xs !! 0 * d, xs !! 1 * d]

    -- Inv function
    backward Inv (Context {savedValues=xs}) d = [(-1 * d) / ((xs !! 0) ** 2)]

    -- Neg function
    backward Neg _ d = [-d]

    -- Sigmoid function
    backward Sigmoid (Context {savedValues=xs}) d = [d * (Operators.sigmoid $ xs !! 0) * (1.0 - (Operators.sigmoid $ xs !! 0))]

    -- ReLU function
    backward ReLU (Context {savedValues=xs}) d = if (xs !! 0) > 0.0 then [d] else [0.0]

    -- LessThan function
    backward LessThan _ _ = [0, 0]

    -- EqualTo function
    backward EqualTo _ _ = [0, 0]

    -- Exp function
    backward Exp (Context {savedValues=xs}) d = [d * (exp $ xs !! 0)]