module Variable where

class Variable a where
    getParents :: a -> Maybe [a]
    isLeaf :: a -> Bool
    isConstant :: a -> Bool
    getUniqueId :: a -> Int
    -- TODO: optimize somehow?
    accumulateDerivative :: Float -> a -> a
    getOutdegree :: a -> Int
    addToPath :: a -> a