module Main where

import Operators
import Module
import Parameter
import Scalar
import Autodiff

main :: IO ()
main = putStrLn (show (isClose 1 1.001))