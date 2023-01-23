module Module where

import Parameter
import Scalar (Scalar)
import qualified Data.Map as Map

data Module a = Module { training :: Bool
                       , submodules :: Map.Map String (Module a)
                       , parameters :: Map.Map String (Parameter a)
                       }
