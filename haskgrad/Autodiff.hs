module Autodiff where

import Variable

import qualified Data.Map as Map

-- Function context to be used in backprop
data Context a = Context {grad :: Bool, savedValues :: [a]} deriving Show

localDerivative :: ([Float] -> Float) -> [Float] -> Int -> Float
localDerivative f xs i = (f1 - (f xs)) / eps
                where f1 = f (take i xs ++ (xs !! i + eps) : drop (i+1) xs)
                      eps = 0.00001

-- Get variables topologically sorted from the right
toVisit :: (Variable a) => a -> Maybe [a]
toVisit x = getParents x >>= (Just . filter (not . isConstant))

-- Should work if no cycles, need to include visited lookup
topologicalSort :: (Variable a) => a -> [a]
topologicalSort x
      | isConstant x = []
      | isLeaf x = [x]
      | otherwise = x : foldl (\c p -> (topologicalSort p) ++ c) [] (case toVisit x of
                                                                              Just parents -> parents
                                                                              Nothing -> [])