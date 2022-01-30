module Main where

import           Data.Geometry                     (Point (..), Vector (..))
import           Data.Geometry.Box                 (Box)
import           Test.AtTheDisco.Color.Combination (combinationTests)
import           Test.AtTheDisco.Geometry.Shapes   (shapeTests)
import           Test.HUnit                        (Test (TestList), runTestTT)

tests :: Test
tests = TestList (shapeTests ++ combinationTests)

main :: IO ()
main = do
  runTestTT tests
  return ()
