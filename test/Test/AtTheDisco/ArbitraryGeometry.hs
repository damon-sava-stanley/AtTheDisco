{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Test.AtTheDisco.ArbitraryGeometry where

import Algorithms.Geometry.ConvexHull (convexHull)
import AtTheDisco.Geometry
import Control.Arrow (Arrow (second))
import Control.Lens (toListOf, view, (^.), (^..))
import Control.Monad (liftM2, replicateM)
import Data.Ext (core, extra, type (:+) ((:+)))
import Data.Foldable (Foldable (toList))
import Data.Function (on)
import Data.Geometry
  ( Affine ((.+^)),
    HasEnd (end),
    HasStart (start),
    IsTransformable (transformBy),
    LineSegment (LineSegment', ClosedLineSegment),
    Point (Point2),
    PointFunctor (pmap),
    PolyLine,
    SimplePolygon,
    Transformation (Transformation),
    Vector,
    qdA,
  )
import qualified Data.Geometry as PG
import Data.Geometry.Box (IsBoxable (boundingBox))
import Data.Geometry.Matrix (Matrix (Matrix))
import qualified Data.Geometry.PolyLine as PL
import qualified Data.Geometry.Polygon as PG
import Data.Geometry.Polygon.Convex (simplePolygon)
import Data.Geometry.Vector.VectorFamily
import Data.Intersection (HasIntersectionWith (intersects))
import Data.List (nubBy, sort, nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromJust, mapMaybe, fromMaybe)
import Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV
import GHC.Generics (Generic)
import System.Random (Random (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary (..),
    Arbitrary1 (..),
    Gen,
    choose,
    chooseInt,
    sized,
    suchThat,
    vector, suchThatMaybe
  )
import qualified Data.LSeq as LSeq

-- Arbitrary Geometreies

points :: [LineSegment 2 p r] -> [Point 2 r :+ p]
points (h : xs) = h ^. start : fmap (^. end) (h : xs)
points _ = []

instance forall p r. (Eq r, Floating r, Arbitrary p, Arbitrary r) => Arbitrary (PolyLine 2 p r) where
  arbitrary = fmap (PL.fromPointsUnsafe . points) c
    where
      c :: Gen [LineSegment 2 p r]
      c =
        sized
          ( \n -> do
              s <- choose (2, n + 2)
              toList . PL.edgeSegments . PL.fromPointsUnsafe <$> vector s
          )
          `suchThat` all (\s -> shapeLength s /= 0)
  shrink = mapMaybe PL.fromPoints . shrink . toList . view PL.points

-- A potentially faster version of length ls >= n, in that it doesn't evaluate the whole list.
lengthAtLeast :: (Ord t, Num t) => t -> [a] -> Bool
lengthAtLeast n [] = n <= 0
lengthAtLeast n (h : xs) = n <= 0 || lengthAtLeast (n - 1) xs

shrinkPolygon :: (Arbitrary p, Arbitrary r) => SimplePolygon p r -> [SimplePolygon p r]
shrinkPolygon p =
  let shrunkPoints = filter (lengthAtLeast 3) . shrink . PG.toPoints $ p
   in fmap PG.unsafeFromPoints shrunkPoints

instance forall p r. (Arbitrary p, Arbitrary r, Floating r, Ord r, Random r) => Arbitrary (SimplePolygon p r) where
  arbitrary = fmap (\(PolygonAroundPoint _ pg) -> pg) arbitrary
  shrink = shrinkPolygon

forceClosed :: LineSegment 2 p r -> LineSegment 2 p r
forceClosed (LineSegment' p r) = ClosedLineSegment p r

instance forall p r. (Arbitrary p, Arbitrary r, Ord r, Floating r, Random r) => Arbitrary (FiniteGeometry p r) where
  arbitrary = do
    t <- chooseInt (0, 2)
    case t of
      0 -> fmap ATDLineSegment (arbitrary :: Gen (LineSegment 2 p r))
      1 -> fmap ATDPolyLine (arbitrary :: Gen (PolyLine 2 p r))
      _ -> fmap ATDSimplePolygon (arbitrary :: Gen (SimplePolygon p r))
  -- that `forceClosed` prevents us from experiencing false negatives with tests
  -- where the endpoints of an open segment are technically not on the list.
  shrink (ATDLineSegment v) = fmap (ATDLineSegment . forceClosed) . shrink $ v
  shrink (ATDPolyLine v) = fmap ATDPolyLine . shrink $ v
  shrink (ATDSimplePolygon v) = fmap ATDSimplePolygon . shrink $ v

instance (Arbitrary p, Arbitrary r, Ord r, Floating r, Random r) => Arbitrary (FiniteGeometries p r) where
  arbitrary = fmap FiniteGeometries arbitrary
  shrink (FiniteGeometries seq) =
    let shrunkGeos = map (FiniteGeometries . LSeq.promise . LSeq.fromList) 
                   . filter (not . null) . mapM shrink . toList $ seq
        shrunkList = map (FiniteGeometries . LSeq.promise . LSeq.fromList) 
                   . filter (not . null) . shrink . toList $ seq
    in shrunkGeos <> shrunkList

instance Arbitrary1 (Transformation 2) where
  liftArbitrary (gen :: Gen a) = Transformation . Matrix <$> genVec3Vec3
    where
      genVec3 :: Gen (Vector 3 a)
      genVec3 = liftArbitrary gen

      genVec3Vec3 :: Gen (Vector 3 (Vector 3 a))
      genVec3Vec3 = liftArbitrary genVec3

instance (Arbitrary a) => Arbitrary (Transformation 2 a) where
  arbitrary = liftArbitrary arbitrary

instance (Arbitrary a, Random a, Ord a, Num a) => Arbitrary (UnitInterval a) where
  arbitrary = unitInterval <$> choose (0, 1)

-- | A type whose arbitrary instances ensure that the polygon are around the point.
data PolygonAroundPoint p r = PolygonAroundPoint (Point 2 r) (SimplePolygon p r) deriving (Show)

polarToVec :: Floating r => r -> r -> Vector 2 r
polarToVec theta r = Vector2 (r * cos theta) (r * sin theta)

instance forall p r. (Arbitrary p, Arbitrary r, Floating r, Random r, Ord r) => Arbitrary (PolygonAroundPoint p r) where
  arbitrary = fromMaybe <$> safePolygonAroundPoint <*> corrected
    where
      base = sized $ \n -> do
        size <- choose (0, n)
        p :: Point 2 r <- arbitrary
        -- ensure that there's a triangle in the hull that contains our point
        angles :: [r] <- sort . ([0, 2*pi/3, 4*pi/3]<>)  <$> replicateM size (choose (1e6, 2 * pi))
        lengths :: [r] <- map ((+1e6) . abs) <$> vector (size + 3)
        annotations :: [p] <- vector (size + 3)
        let offsets = zipWith polarToVec angles lengths
        let points = map (p .+^) offsets
        let annotated = zipWith (:+) points annotations
        return $ PolygonAroundPoint p (PG.fromPoints annotated)
      -- Shouldn't need this such that, but sometimes floating point nonsense strikes. Should use an arbitrary position type,
      -- but those aren't floating. (I've hacked this with `UR`, which kind of works)
      corrected = base `suchThatMaybe` (\(PolygonAroundPoint p pg)-> PG.insidePolygon p pg)
      -- A nice box containing our polygon. Should only be used if things go really bad
      safePolygonAroundPoint = do
        c <- arbitrary
        return $ PolygonAroundPoint (Point2 0 0) (PG.fromPoints [Point2 10 10 :+ c, Point2 (-10) 10 :+ c, 
                                                                 Point2 (-10) (-10) :+ c, Point2 10 (-10) :+ c])
  shrink (PolygonAroundPoint p pg) =  
    let pgs = PG.insidePolygon p `filter` shrinkPolygon pg
    in fmap (PolygonAroundPoint p) pgs