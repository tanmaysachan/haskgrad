module Variable where

class Variable a where
    getParents :: a -> Maybe [a]
    isLeaf :: a -> Bool
    isConstant :: a -> Bool