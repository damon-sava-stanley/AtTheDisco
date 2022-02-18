{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module : AtTheDisco.Geometry
-- Description : Extensions of "Data.Geometry" suited for drawing.
-- Copyright : (c) Damon Stanley, 2022
-- License : BSD3
-- Maintainer : damonsava@gmail.com
-- Stability : experimental
--
-- Contains the geometric types of /AtTheDisco/. In particular 'FiniteGeometry' wraps particular bits of finite geometry
-- and 'FiniteGeometries' collects them into assemblies. Probably you want to use the convenience functions in
-- "AtTHeDisco.Builder" to actually build these.
module AtTheDisco.Geometry
  ( -- * Finite Geometry #fg#
    -- $finiteGeometry
    FiniteGeometry (..),

    -- * Geometry Additions #ga#
    -- $extensionFiniteGeometry
    UnitInterval,
    unitInterval,
    viewUnitInterval,
    Interpolable (..),
    HasLength (..),
    HasInside (..),
    Projectable (..),

    -- * Finite Geometries #fgs#
    -- $finiteGeometries
    FiniteGeometries (FiniteGeometries),
    unwrapFiniteGeometries,
  )
where

import Control.Lens
  ( Bifunctor (..),
    makeLenses,
    over,
    view,
    (&),
    (.~),
    (^.),
  )
import Data.Ext (core, extra, type (:+) (..))
import Data.Foldable (Foldable (toList), minimumBy)
import Data.Function (on)
import Data.Geometry
  ( Dimension,
    HasEnd (end),
    HasIntersectionWith (..),
    HasStart (start),
    IntersectionOf,
    IsTransformable (..),
    LineSegment (LineSegment', ClosedLineSegment),
    NoIntersection,
    NumType,
    Point,
    PointFunctor (..),
    PolyLine,
    Polygon,
    SimplePolygon,
    edgeSegments,
    fromPointsUnsafe,
    insidePolygon,
    interpolate,
    listEdges,
    segmentLength,
    sqDistanceToSegArg,
  )
import Data.Geometry.Ball (Disk)
import Data.Geometry.Boundary (PointLocationResult (Inside, Outside))
import Data.Geometry.Box
  ( IsBoxable (boundingBox),
    Rectangle,
    boundingBoxList,
  )
import qualified Data.Geometry.PolyLine as PL
import qualified Data.Geometry.Polygon as PG
import Data.Geometry.Properties (IsIntersectableWith (intersect))
import Data.Geometry.Transformation (IsTransformable (transformBy))
import Data.Intersection (IsIntersectableWith (nonEmptyIntersection))
import Data.LSeq (LSeq, toSeq)
import qualified Data.Sequence as Seq
import Data.Vinyl.CoRec (CoRec (CoRec))
import GHC.Generics (Generic)

-- $finiteGeometry
--
-- Defines a sum type ('FiniteGeometry') for wrapping various bits of 'Data.Geometry' that fit into boxes and can be transformed.

-- | 'FiniteGeometry' is geometry that can be transformed ('PointFunctor' and 'IsTransformable')
--   and takes up finite space, i.e. 'IsBoxable'.
data FiniteGeometry p r
  = ATDLineSegment (LineSegment 2 p r)
  | ATDPolyLine (PolyLine 2 p r)
  | ATDSimplePolygon (SimplePolygon p r)
  deriving (Generic, Eq, Show)

instance Bifunctor FiniteGeometry where
  bimap ef cf (ATDLineSegment ls) = ATDLineSegment $ bimap ef cf ls
  bimap ef cf (ATDPolyLine ls) = ATDPolyLine $ bimap ef cf ls
  bimap ef cf (ATDSimplePolygon ls) = ATDSimplePolygon $ bimap ef cf ls

type instance Dimension (FiniteGeometry p r) = 2

type instance NumType (FiniteGeometry p r) = r

instance PointFunctor (FiniteGeometry p) where
  pmap f (ATDLineSegment v) = ATDLineSegment $ pmap f v
  pmap f (ATDPolyLine v) = ATDPolyLine $ pmap f v
  pmap f (ATDSimplePolygon v) = ATDSimplePolygon $ pmap f v

instance (Fractional r) => IsTransformable (FiniteGeometry p r) where
  transformBy t (ATDLineSegment v) = ATDLineSegment $ transformBy t v
  transformBy t (ATDPolyLine v) = ATDPolyLine $ transformBy t v
  transformBy t (ATDSimplePolygon v) = ATDSimplePolygon $ transformBy t v

instance IsBoxable (FiniteGeometry p r) where
  boundingBox (ATDLineSegment v) = boundingBox v
  boundingBox (ATDPolyLine v) = boundingBox v
  boundingBox (ATDSimplePolygon v) = boundingBox v

type instance IntersectionOf (Point 2 r) (PolyLine 2 p r) = '[NoIntersection, Point 2 r]

type instance IntersectionOf (Point 2 r) (FiniteGeometry p r) = '[NoIntersection, Point 2 r]

instance (Num r, Ord r) => HasIntersectionWith (Point 2 r) (PolyLine 2 p r) where
  intersects p pl = any (\ls -> p `intersects` ls) (edgeSegments pl)

instance (Fractional r, Ord r) => HasIntersectionWith (Point 2 r) (FiniteGeometry p r) where
  intersects p (ATDLineSegment v) = intersects p v
  intersects p (ATDPolyLine v) = intersects p v
  intersects p (ATDSimplePolygon v) = intersects p v

-- $extensionFiniteGeometry
--
-- Classes which extend the functionality of "Data.Geometry"

-- | A 'UnitInterval' carries a value in the inclusive range [0, 1].
newtype UnitInterval a = UnitInterval a deriving (Generic, Eq, Ord, Show)

unitInterval :: (Ord a, Num a) => a -> UnitInterval a
unitInterval x
  | x < 0 = UnitInterval 0
  | x > 1 = UnitInterval 1
  | otherwise = UnitInterval x

viewUnitInterval :: UnitInterval a -> a
viewUnitInterval (UnitInterval a) = a

polygonToPolyLine :: Polygon t p r -> PolyLine 2 p r
polygonToPolyLine = PL.fromPointsUnsafe . PG.toPoints

-- | An 'Interpolable' shape is one whose boundary can be indexed by a number on the unit interval,
-- and which, further, can be divided at a certain point. These should satisfy the following laws.
--
-- Assuming, as should be true, that the result of 'splitInterp' is itself 'Interpolable'
--
-- prop> interp x s = interp 1 (fst (splitInterp x s)
-- prop> interp x s = interp 0 (snd (splitInterp x s)
-- prop> interp 0 s = interp 0 (fst (splitInterp x s)
-- prop> interp 1 s = interp 1 (snd (splitInterp x s))
--
-- More generally. For \(x < y\),
--
-- prop> interp x s = interp (x/y) (fst (splitInterp y) s)
-- prop> interp y s = interp ((y-x)/(1-x)) (snd (splitInterp x) s)
--
-- Assuming that the shape 'HasLength'.
--
-- prop> shapeLength (fst (splitInterp x s)) = viewUnitInterval x * shapeLength s
-- prop> shapeLength (snd (splitInterp x s)) = (1 - viewUnitInterval x) * shapeLength s
--
-- Assuming that the shape 'IsIntersectableWith' 'Point' 2 r, then
--
-- prop> intersects (interp x s) s
--
-- Finally, the function 'interpAndSplit' is offered as a potentially more convenient to implement
-- bundle, which may also reduce wasted computation in case both interpolation and splitting are needed.
--
-- Minimum implementation: either 'interpAndSplit' or 'interp' and 'splitInterp'
class Interpolable f r where
  -- | The result of splitting a shape into two parts. Should itself be 'Interpolable'; may be the
  --   original type.
  type InterpolationSplit f :: * -> *

  -- | Performs both 'interp' and 'splitInterp' at a given interpolation percent.
  interpAndSplit :: UnitInterval r -> f r -> (Point 2 r, (InterpolationSplit f r, InterpolationSplit f r))
  interpAndSplit u r = (interp u r, splitInterp u r)

  -- | Find a point on the shape that is a given percent along the boundary.
  interp :: UnitInterval r -> f r -> Point 2 r
  interp u r = fst (interpAndSplit u r)

  -- | Divide a shape into two sub-shapes, one of which contains the boundary.
  -- Warning: values of 0 and 1 may produce pathological results.
  splitInterp :: UnitInterval r -> f r -> (InterpolationSplit f r, InterpolationSplit f r)
  splitInterp u r = snd (interpAndSplit u r)

instance (Fractional r) => Interpolable (LineSegment 2 p) r where
  type InterpolationSplit (LineSegment 2 p) = LineSegment 2 p
  interp (UnitInterval x) ls = interpolate x ls
  splitInterp (UnitInterval x) ls = (left, right)
    where
      p = interpolate x ls
      -- TODO: This doesn't properly set the Open/Closed state. Don't know that it will matter
      left = ls & end . core .~ p
      right = ls & start . core .~ p

instance forall r p. (Ord r, Floating r) => Interpolable (PolyLine 2 p) r where
  type InterpolationSplit (PolyLine 2 p) = PolyLine 2 p

  interpAndSplit u pl = go u pl
    where
      go :: UnitInterval r -> PolyLine 2 p r -> (Point 2 r, (PolyLine 2 p r, PolyLine 2 p r))
      go u pl =
        let (p, sl, sr) = go' (viewUnitInterval u * shapeLength pl) [] (toList . edgeSegments $ pl)
         in (p, (fromPointsUnsafe (points sl), fromPointsUnsafe (points sr)))

      points :: [LineSegment 2 p r] -> [Point 2 r :+ p]
      points [] = []
      points (h : xs) = h ^. start : map (^. end) (h : xs)

      go' :: r -> [LineSegment 2 p r] -> [LineSegment 2 p r] -> (Point 2 r, [LineSegment 2 p r], [LineSegment 2 p r])
      go' target past (current : future) =
        let l = shapeLength current
         in if target <= l
              then
                let (p, (sl, sr)) = interpAndSplit (unitInterval (target / l)) current
                 in (p, reverse (sl : past), sr : future)
              else go' (target - l) (current : past) future
      go' _ _ [] = error "Should not run off the edge in Polyline interpAndSplit"

instance (Ord r, Floating r) => Interpolable (SimplePolygon p) r where
  type InterpolationSplit (SimplePolygon p) = PolyLine 2 p

  interpAndSplit u pg = interpAndSplit u (polygonToPolyLine pg)

instance (Ord r, Floating r) => Interpolable (FiniteGeometry p) r where
  type InterpolationSplit (FiniteGeometry p) = FiniteGeometry p

  interpAndSplit x (ATDLineSegment ls) = let (p, (lsS, lsE)) = interpAndSplit x ls in (p, (ATDLineSegment lsS, ATDLineSegment lsE))
  interpAndSplit x (ATDPolyLine ls) = let (p, (lsS, lsE)) = interpAndSplit x ls in (p, (ATDPolyLine lsS, ATDPolyLine lsE))
  interpAndSplit x (ATDSimplePolygon ls) = let (p, (lsS, lsE)) = interpAndSplit x ls in (p, (ATDPolyLine lsS, ATDPolyLine lsE))

-- | A shape 'HasLength' when it has a length. Always nonnegative.
class HasLength f r where
  -- | The length of the shape.
  shapeLength :: f r -> r

instance (Floating r) => HasLength (LineSegment 2 p) r where
  shapeLength = segmentLength

instance (Floating r) => HasLength (PolyLine 2 p) r where
  shapeLength = foldr (\ls acc -> acc + shapeLength ls) 0 . edgeSegments

instance (Floating r) => HasLength (SimplePolygon p) r where
  shapeLength = shapeLength . polygonToPolyLine

instance (Floating r) => HasLength (FiniteGeometry p) r where
  shapeLength (ATDLineSegment ls) = shapeLength ls
  shapeLength (ATDPolyLine pl) = shapeLength pl
  shapeLength (ATDSimplePolygon pg) = shapeLength pg

-- | Shapes which have an inside. Note that 1D shape have trivial insides, i.e. nothing is inside them.
class HasInside f p r where
  isInside :: Point 2 r -> f p r -> Bool

instance HasInside (LineSegment 2) p r where
  isInside p v = False

instance HasInside (PolyLine 2) p r where
  isInside p v = False

instance (Fractional r, Ord r) => HasInside SimplePolygon p r where
  isInside = insidePolygon

instance (Fractional r, Ord r) => HasInside FiniteGeometry p r where
  isInside p (ATDLineSegment v) = isInside p v
  isInside p (ATDPolyLine v) = isInside p v
  isInside p (ATDSimplePolygon v) = isInside p v

-- | To "project" a point on to a shape is to find the point on the boundary of the shape closest to the given point.
class Projectable f p r where
  -- | Find the point on the shape closest to the given point.
  project ::
    -- | the point to query
    Point 2 r ->
    -- | the shape
    f p r ->
    -- | a pair of the distance squared to the projected point and the projected point
    (r, Point 2 r)

instance (Fractional r, Ord r) => Projectable (LineSegment 2) p r where
  -- Bodge to work around https://github.com/noinia/hgeometry/issues/170
  project p (LineSegment' s e) = sqDistanceToSegArg p (ClosedLineSegment s e)

-- Making this a `Projectable` instance would require some type level composition I don't feel like wrestling with.
projectNonEmptyFoldableLineSegments ::
  (Foldable t, Ord a, Functor t, Projectable f p a) =>
  Point 2 a ->
  t (f p a) ->
  (a, Point 2 a)
projectNonEmptyFoldableLineSegments p =
  minimumBy (compare `on` fst) . fmap (project p)

instance (Fractional r, Ord r) => Projectable (PolyLine 2) p r where
  project p = projectNonEmptyFoldableLineSegments p . toSeq . edgeSegments

instance (Fractional r, Ord r) => Projectable SimplePolygon p r where
  project p poly = projectNonEmptyFoldableLineSegments p . listEdges $ poly

instance (Fractional r, Ord r) => Projectable FiniteGeometry p r where
  project p (ATDLineSegment l) = project p l
  project p (ATDPolyLine l) = project p l
  project p (ATDSimplePolygon l) = project p l

-- $finiteGeometries
--
-- It is handy to deal with collection of shapes, which is what 'FiniteGeometries' provides. It has much of the same functionality,
-- but because it is not a single shape.

-- | `FiniteGeometries` represents a non-empty collection of `FiniteGeometry`. When this matters, objects earlier
--   in the sequence are considered on top.
newtype FiniteGeometries p r = FiniteGeometries
  {_unwrapFiniteGeometries :: LSeq 1 (FiniteGeometry p r)}
  deriving (Eq, Show, Generic)

$(makeLenses ''FiniteGeometries)

type instance Dimension (FiniteGeometries p r) = 2

type instance NumType (FiniteGeometries p r) = r

instance Bifunctor FiniteGeometries where
  bimap f g (FiniteGeometries s) = FiniteGeometries (fmap (bimap f g) s)

instance PointFunctor (FiniteGeometries p) where
  pmap f = over unwrapFiniteGeometries (fmap (pmap f))

instance IsBoxable (FiniteGeometries p r) where
  boundingBox = boundingBoxList . view unwrapFiniteGeometries

instance (Fractional r, Ord r) => Projectable FiniteGeometries p r where
  project p = projectNonEmptyFoldableLineSegments p . view unwrapFiniteGeometries

instance (Fractional r, Ord r) => HasInside FiniteGeometries p r where
  isInside r = any (isInside r) . view unwrapFiniteGeometries

-- No `Monoid` as we are requiring nonempty.
instance Semigroup (FiniteGeometries p r) where
  (FiniteGeometries l1) <> (FiniteGeometries l2) = FiniteGeometries (l1 <> l2)