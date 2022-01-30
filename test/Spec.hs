module Main where

import           Data.Geometry                     (Point (..), Vector (..))
import           Data.Geometry.Box                 (Box)
import           Test.AtTheDisco.Color.Combination (combinationTests)
import           Test.AtTheDisco.Color.Gradient    (gradientTests)
import           Test.AtTheDisco.Geometry.Gradient (gradientShapesTests)
import           Test.AtTheDisco.Geometry.Shapes   (shapeTests)
import           Test.HUnit                        (Test (TestList), runTestTT)

tests :: Test
tests =
  TestList
    (shapeTests ++ combinationTests ++ gradientTests ++ gradientShapesTests)

main :: IO ()
main = do
  runTestTT tests
  return ()
