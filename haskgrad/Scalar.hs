module Scalar where

import System.Random
import Control.Monad

import Variable
import Autodiff
import qualified ScalarFunction

data ScalarHistory = ScalarHistory { lastFn :: Maybe ScalarFunction.Functions
                                   , ctx :: Context Float
                                   , inputs :: Maybe [Scalar]
                                   } deriving Show

data Scalar = Scalar { uniqueId :: Int
                     , value :: Float
                     , derivative :: Float
                     , history :: Maybe ScalarHistory 
                     } deriving Show

-- UniqueID through randgen :(?
genUniqueId :: Float -> Int
genUniqueId x = fst $ randomR (1,1000000) (mkStdGen (floor (1000*x)))

instance Variable Scalar where
    --                                  ScalarHistory inputs
    getParents Scalar {history = h} = h >>= inputs

    -- Scalar is a leaf if history exists, but lastFn is Nothing
    isLeaf Scalar {history = Just (ScalarHistory {lastFn = Nothing})} = True
    isLeaf _ = False

    -- Scalar is constant if no history
    isConstant Scalar {history = Nothing} = True
    isConstant _ = False

    getUniqueId Scalar {uniqueId = x} = x


-- Create scalar node on function application
data ScalarLike =  Slf Float | Sls Scalar

-- Create scalar with value, history
initScalar :: Float -> Maybe ScalarHistory -> Scalar
initScalar v hist = Scalar { uniqueId = genUniqueId v
                           , value = v
                           , derivative = 0.0
                           , history = hist
                           }

getScalarHistory :: ScalarFunction.Functions -> Context Float -> Maybe [Scalar] -> ScalarHistory
getScalarHistory f ctx xs = ScalarHistory { lastFn = Just f
                                          , ctx = ctx
                                          , inputs = xs
                                          }

fmapScalars :: Maybe [ScalarLike] -> Maybe [Scalar]
fmapScalars xs = xs >>= (return . fmap (\s -> case s of   -- Possible fuckup => Empty ScalarHistory -> Leaf node, Nothing ScalarHistory -> Constant 
                                                Slf x -> initScalar x (Just $ ScalarHistory { lastFn=Nothing
                                                                                            , ctx=Context {grad=True, savedValues=[]}
                                                                                            , inputs=Nothing})
                                                Sls x -> x))

-- Pattern matching on only 1 var
extractArg :: [ScalarLike] -> Maybe [Float]
extractArg [Slf x] = Just [x]
extractArg [Sls x] = Just [value x]

-- One compulsory var, rest optional
applyFn1 :: ScalarFunction.Functions -> Scalar -> Maybe [ScalarLike] -> (Context Float, Float)
applyFn1 f x ys = (ScalarFunction.forward f (value x) (ys >>= extractArg))

applyFn :: ScalarFunction.Functions -> Scalar -> Maybe [ScalarLike] -> Scalar
applyFn f x ys = initScalar (snd $ applyFn1 f x ys) (Just $ getScalarHistory f (fst $ applyFn1 f x ys) (fmapScalars $ case ys of
                                                                                                                        Just ys' -> Just $ (Sls x):ys'
                                                                                                                        Nothing -> Just [Sls x]))
