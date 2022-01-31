{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Functions for turning shapes into layers.
module AtTheDisco.Geometry.Shapes where

import           AtTheDisco.Layer       (Layer, ScreenLayer)
import           Control.Lens           ((^.))
import           Control.Lens.TH        (makeLenses)
import           Data.Geometry          (Arity, Line, LineSegment, Point,
                                         Polygon, inPolygon, sqDistanceTo,
                                         sqDistanceToSeg)
import           Data.Geometry.Ball     (Ball, inClosedBall)
import           Data.Geometry.Boundary (PointLocationResult (Inside, OnBoundary))
import           Data.Geometry.Box      (Box, inBox)
import           Data.Geometry.Matrix   (Invertible)
import           Data.Geometry.Polygon  (Polygon, insidePolygon)

data Thick2DLine r = Thick2DLine
    { _thickLine     :: Line 2 r
    , _lineThickness :: r
    }
    deriving (Eq, Show)

$(makeLenses ''Thick2DLine)

data Thick2DLineSegment r = Thick2DLineSegment
    { _thickLineSegment     :: LineSegment 2 () r
    , _lineSegmentThickness :: r
    }
    deriving (Eq, Show)

$(makeLenses ''Thick2DLineSegment)

class Drawable v e where
  draw :: v e -> ScreenLayer (Point 2) e

instance Ord e => Drawable (Box 2 p) e where
  draw box point = point `inBox` box

instance (Ord e, Num e) => Drawable (Ball 2 p) e where
  draw ball point = point `inClosedBall` ball

instance (Fractional e, Ord e) => Drawable (Polygon t p) e where
  draw poly point = (point `inPolygon` poly) `elem` [Inside, OnBoundary]

instance (Fractional e, Ord e) => Drawable Thick2DLine e where
  draw (Thick2DLine l r) point = sqDistanceTo point l <= r ^^ 2

instance (Fractional e, Ord e) => Drawable Thick2DLineSegment e where
  draw (Thick2DLineSegment s r) p = sqDistanceToSeg p s <= r ^^ 2
