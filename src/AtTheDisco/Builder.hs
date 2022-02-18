{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
module AtTheDisco.Builder where

import AtTheDisco.Geometry
import Data.Foldable (Foldable (toList))
import qualified Data.Geometry.Polygon as PG
import qualified Data.LSeq as LSeq
import Data.Ext ( type (:+)((:+)) )
import Data.Geometry

fgSingle :: FiniteGeometry c r -> FiniteGeometries c r
fgSingle fg = FiniteGeometries (fg LSeq.<| LSeq.empty)

toPoint2 :: (r, r) -> Point 2 r
toPoint2 = uncurry Point2

pgon :: (Foldable f, Ord r, Fractional r) => f (r, r) -> FiniteGeometries () r
pgon ps = fgSingle (ATDSimplePolygon pg) 
  where
    points =  map ((:+ ()) . toPoint2) . toList $ ps 
    pg = PG.simpleFromPoints points