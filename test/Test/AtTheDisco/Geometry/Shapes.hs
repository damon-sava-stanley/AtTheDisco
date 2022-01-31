module Test.AtTheDisco.Geometry.Shapes where

import           Test.HUnit                        (Test, (~?))

import           AtTheDisco.Geometry.Shapes        (Drawable (draw))
import           Data.Ext                          ((:+) ((:+)))
import           Data.Geometry                     (Point (..), Vector (..))
import qualified Data.Geometry.Ball                as Ball
import           Data.Geometry.Box                 (Box)
import qualified Data.Geometry.Box                 as Box
import           Data.Geometry.Polygon             (fromPoints)
import           Data.Geometry.Vector.VectorFamily


shapeTests :: [Test]
shapeTests =
  [ centerInBox
  , cornerInBox
  , outsideNotInBox
  , centerInBall
  , edgeInBall
  , outsideNotInBall
  , centerInPolygon
  , cornerInPolygon
  ]

centerInBox :: Test
centerInBox =
  let center = Point2 50 50
      smallBox = Box.fromCenter center (Vector2 40 40)
   in draw smallBox center ~? "Center in Box"

cornerInBox :: Test
cornerInBox =
  let center = Point2 50 50
      smallBox = Box.fromCenter center (Vector2 40 40)
      corner = Point2 70 70
   in draw smallBox corner ~? "Edge in Box"

outsideNotInBox :: Test
outsideNotInBox =
  let center = Point2 50 50
      smallBox = Box.fromCenter center (Vector2 40 40)
      outside = Point2 71 71
   in not (draw smallBox outside) ~? "Edge in Box"

centerInBall =
  let center = Point2 0 0
      smallBall = Ball.fromDiameter (Point2 (-20) (-20)) (Point2 20 20)
   in draw smallBall center ~? "Center in Ball"

edgeInBall =
  let edge = Point2 20 20
      smallBall = Ball.fromDiameter (Point2 (-20) (-20)) (Point2 20 20)
   in draw smallBall edge ~? "Edge in Ball"

outsideNotInBall =
  let smallBall = Ball.fromDiameter (Point2 (-20) (-20)) (Point2 20 20)
   in not (draw smallBall (Point2 21 21)) ~? "Outside not in Ball"

centerInPolygon =
  let center = Point2 0 0
      rectangle =
        fromPoints . map (:+ ()) $
        [Point2 (-20) (-20), Point2 20 (-20), Point2 20 20, Point2 (-20) 20]
   in draw rectangle center ~? "Center in Polygon"

cornerInPolygon =
  let edge = Point2 20 0
      rectangle =
        fromPoints . map (:+ ()) $
        [Point2 (-20) (-20), Point2 20 (-20), Point2 20 20, Point2 (-20) 20]
   in draw rectangle edge ~? "Edge in Polygon"

outsideNotInPolygon =
  let outside = Point2 21 0
      rectangle =
        fromPoints . map (:+ ()) $
        [Point2 (-20) (-20), Point2 20 (-20), Point2 20 20, Point2 (-20) 20]
   in not (draw rectangle outside) ~? "Outside not in Polygon"
