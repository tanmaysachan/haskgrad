import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Autodiff

closeTo :: Float -> Float -> Bool
closeTo x y
        | (abs (x-y)) < 0.01 = True
        | otherwise = False

main :: IO()
main = hspec $ do
    describe "Autodiff" $ do
        it "Checks derivative of a small function" $ do
            localDerivative (foldr (*) 1) [4.0, 5.0] 1 `shouldSatisfy` closeTo 4.0