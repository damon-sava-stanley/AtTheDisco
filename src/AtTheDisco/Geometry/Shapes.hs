{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE MonoLocalBinds #-}

module AtTheDisco.Geometry.Shapes
  ( boxLayer
  , ballLayer
  , polygonLayer
  ) where

import           AtTheDisco.Layer       (ScreenLayer)
import           Data.Geometry          (Arity, Point, inPolygon)
import           Data.Geometry.Ball     (Ball, inClosedBall)
import           Data.Geometry.Boundary (PointLocationResult (Inside, OnBoundary))
import           Data.Geometry.Box      (Box, inBox)
import           Data.Geometry.Polygon  (Polygon, insidePolygon)

boxLayer :: (Arity d, Ord r) => Box d p r -> ScreenLayer (Point d) r
boxLayer box point = point `inBox` box

ballLayer :: (Arity d, Ord r, Num r) => Ball d p r -> ScreenLayer (Point d) r
ballLayer ball point = point `inClosedBall` ball

polygonLayer ::
     (Fractional r, Ord r) => Polygon t p r -> ScreenLayer (Point 2) r
polygonLayer poly point = (point `inPolygon` poly) `elem` [Inside, OnBoundary]
