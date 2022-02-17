{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Test.AtTheDisco.ArbitraryGeometry where

import AtTheDisco.Geometry
import Control.Arrow (Arrow (second))
import Control.Lens (toListOf, view, (^.), (^..))
import Control.Monad (liftM2)
import Data.Ext (core, extra, type (:+))
import Data.Foldable (Foldable (toList))
import Data.Geometry
  ( HasEnd (end),
    HasStart (start),
    IsTransformable (transformBy),
    LineSegment (LineSegment'),
    Point (Point2),
    PointFunctor (pmap),
    PolyLine,
    SimplePolygon,
    Transformation (Transformation),
    Vector,
    qdA,
  )
import Data.Geometry.Box (IsBoxable (boundingBox))
import Data.Geometry.Matrix (Matrix (Matrix))
import qualified Data.Geometry.PolyLine as PL
import qualified Data.Geometry.Polygon as PG
import Data.Intersection (HasIntersectionWith (intersects))
import Data.List (nubBy)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV
import GHC.Generics (Generic)
import System.Random (Random (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Arbitrary(..),
      Gen,
      vector,
      choose,
      chooseInt,
      sized,
      suchThat,
      Arbitrary1(..) )

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

instance Arbitrary1 CircularVector where
  liftArbitrary gen = sized $ \n -> do
    l <- choose (1, n)
    CV.replicate1M l gen
  liftShrink s v = mapMaybe (CV.fromList . s) . CV.toList $ v

instance forall p r. (Arbitrary p, Arbitrary r, Eq r, Num r) => Arbitrary (SimplePolygon p r) where
  arbitrary = fmap PG.fromCircularVector cv3Uniq
    where
      cv3 :: Gen (CircularVector (Point 2 r :+ p))
      cv3 = CV.cons <$> arbitrary <*> (CV.cons <$> arbitrary <*> arbitrary)
      cv3Uniq :: Gen (CircularVector (Point 2 r :+ p))
      cv3Uniq =
        cv3
          `suchThat` ( isJust
                         . foldr
                           ( \a m -> do
                               b <- m
                               if b /= a ^. core then Just (a ^. core) else Nothing
                           )
                           (Just (Point2 0 0))
                     )
      isJust (Just _) = True
      isJust Nothing = False
  shrink = fmap PG.fromCircularVector . shrink . fromJust . CV.fromVector' . PG.toVector

instance forall p r. (Arbitrary p, Arbitrary r, Eq r, Floating r) => Arbitrary (FiniteGeometry p r) where
  arbitrary = do
    t <- chooseInt (0, 2)
    case t of
      0 -> fmap ATDLineSegment (arbitrary :: Gen (LineSegment 2 p r))
      1 -> fmap ATDPolyLine (arbitrary :: Gen (PolyLine 2 p r))
      _ -> fmap ATDSimplePolygon (arbitrary :: Gen (SimplePolygon p r))
  shrink (ATDLineSegment v) = fmap ATDLineSegment . shrink $ v
  shrink (ATDPolyLine v) = fmap ATDPolyLine . shrink $ v
  shrink (ATDSimplePolygon v) = fmap ATDSimplePolygon . shrink $ v

instance forall p r. (Arbitrary p, Arbitrary r, Eq r, Floating r) => Arbitrary (FiniteGeometries p r) where
  arbitrary = fmap FiniteGeometries arbitrary
  shrink (FiniteGeometries seq) = fmap FiniteGeometries . shrink $ seq

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