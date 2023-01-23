module Parameter where

import Scalar (Scalar)
import Variable

data Parameter a = Parameter { value :: a
                             , name :: String
                             }

initParameter :: Scalar -> String -> Parameter Scalar
initParameter s n = Parameter { value = s
                              , name = n
                              }