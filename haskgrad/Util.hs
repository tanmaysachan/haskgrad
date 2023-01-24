module Util where

import System.Random
import qualified Data.Map as Map
import Data.List
import Control.Lens

import Variable

-- UniqueID through randgen :(?
genUniqueId :: Float -> Int
genUniqueId x = fst $ randomR (1,1000000) (mkStdGen (floor (1000*x)))


-- Topo sort utils
toVisit :: (Variable a) => a -> [a]
toVisit x = case getParents x >>= (Just . filter (not . isConstant)) of
                  Just res -> res
                  Nothing -> []

-- Return var and index
chosenV :: (Variable a) => [a] -> [Int] -> (Maybe a, Int)
chosen [] _ = (Nothing, -1)
chosen _ [] = (Nothing, -1)
chosenV vs is = (vs ^? element i, i)
            where i = case elemIndex (foldl1' min is) is of
                        Just x -> x
                        Nothing -> -1

varToVisitUtil :: (Variable a) => [a] -> (Maybe a, Int)
varToVisitUtil xs = chosenV xs (map getOutdegree xs)

-- Input - rightmost variable, possible next variables
topologicalSortMain :: (Variable a) => Maybe a -> [a] -> [a]
topologicalSortMain Nothing xs = []
topologicalSortMain (Just x) xs
      | isConstant x = []
      | isLeaf x = [x]
      | otherwise = x : topologicalSortMain varToVisit newCandidates
            where varToVisitTup = varToVisitUtil allNodes
                  indexToRem = snd varToVisitTup
                  varToVisit = fst varToVisitTup
                  newCandidates = take indexToRem allNodes ++ drop (indexToRem + 1) allNodes
                  allNodes = xs ++ toVisit x
