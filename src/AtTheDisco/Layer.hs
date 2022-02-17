{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AtTheDisco.Layer
  ( -- * Drawing Geometry
    -- $drawingGeometry
    GeometryStyle (GeometryStyle),
    geometryThickness,
    geometryLineColor,
    geometryFill,
    Drawable (..),
  )
where

import AtTheDisco.Geometry
  ( FiniteGeometries,
    HasInside (getInside),
    Projectable (project),
  )
import Control.Lens (over, (^.))
import Control.Lens.TH (makeLenses)
import Data.Ext (type (:+) ((:+)))
import Data.Geometry (Dimension, NumType, Point)
import Data.Geometry.Box (IsBoxable (boundingBox))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)

-- $drawingGeometry
--
-- Given an assembly of shapes, see "AtTheDisco.Geometry", provide functions for coloring those
-- shapes and producing a Pan-style continuous image.

-- | Style information for coloring a piece of geometry.
data GeometryStyle r c = GeometryStyle
  { -- | The radius of the lines.
    _geometryThickness :: r,
    -- | The color of the line. `Nothing` if invisble (should be `Nothing` if the diameter is 0).
    _geometryLineColor :: Maybe c,
    -- | How this should be filled. Can be an arbitrary pattern.
    --   Two layers of maybe: the fill as a whole could be `Nothing`, but if it is
    --   not the colors should help.
    _geometryFill :: Maybe (Point 2 r -> Maybe c)
  }
  deriving (Functor)

$(makeLenses ''GeometryStyle)

instance (Show r, Show c) => Show (GeometryStyle r c) where
  show (GeometryStyle t lc f) =
    "GeometryStyle " <> show t <> " " <> show lc <> " "
      <> (case f of Just _ -> "Just _"; Nothing -> "Nothing")

-- | A `Drawable` shape can be samples for color at any point.
class Drawable f r c where
  draw :: f r -> Point 2 r -> c

instance
  (Ord r, Num r, HasInside f (GeometryStyle r c) r, Projectable f (GeometryStyle r c) r) =>
  Drawable (f (GeometryStyle r c)) r (Maybe c)
  where
  draw shape point = lineColor `firstJust` insideColor
    where
      (sqDist, p :+ cfg) = project point shape
      r = cfg ^. geometryThickness
      lineColor = if sqDist <= r * r then cfg ^. geometryLineColor else Nothing
      inside = getInside point shape
      insideColor = do
        cfg' <- inside
        let fill = cfg' ^. geometryFill
        fill' <- fill
        fill' point
      firstJust (Just x) _ = Just x
      firstJust _ y = y
