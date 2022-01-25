module Main where

import Data.Geometry (Point (..), Vector (..))
import Data.Geometry.Box (Box)
import Test.AtTheDisco.Geometry.Shapes (shapeTests)
import Test.HUnit

tests =
  TestList
    shapeTests

main :: IO ()
main = do
  runTestTT tests
  return ()