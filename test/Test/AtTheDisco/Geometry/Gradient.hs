module Test.AtTheDisco.Geometry.Gradient where

import           AtTheDisco.Geometry.Gradient (lineSegmentGradient)
import           Data.Geometry
import Test.HUnit ( (~:), (~=?), (~?=) )

gradientShapesTests = [lineSegmentGradientTest, lineSegmentGradientTest0, lineSegmentGradientTest1, lineSegmentGradientTest2, lineSegmentGradientTestN1]

-- TODO: Unify these into a property test
lineSegmentGradientTest =
  "lineSegmentGradient 0.5" ~:
  lineSegmentGradient (Point2 0 0) (Vector2 1 0) (Point2 0.5 500) ~?=
  0.5

lineSegmentGradientTest0 =
  "lineSegmentGradient 0" ~:
  lineSegmentGradient (Point2 0 0) (Vector2 1 0) (Point2 0 500) ~?=
  0

lineSegmentGradientTest1 =
  "lineSegmentGradient ignores vertical" ~:
  lineSegmentGradient (Point2 0 0) (Vector2 1 0) (Point2 1 500) ~?=
  1

lineSegmentGradientTest2 =
  "lineSegmentGradient ignores vertical" ~:
  lineSegmentGradient (Point2 0 0) (Vector2 1 0) (Point2 2 500) ~?=
  2

lineSegmentGradientTestN1 =
  "lineSegmentGradient ignores vertical" ~:
  lineSegmentGradient (Point2 0 0) (Vector2 1 0) (Point2 (-1) 500) ~?=
  (-1)

