{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.AtTheDisco.GeometrySpec (spec) where

import Test.AtTheDisco.ArbitraryGeometry()
import AtTheDisco.Geometry
import Control.Arrow (Arrow (second))
import Control.Lens (toListOf, view, (^.), (^..))
import Control.Monad (liftM2)
import Data.Ext (core, extra, type (:+))
import Data.Foldable (Foldable (toList))
import Data.Geometry
  ( HasIntersectionWith (intersects),
    IsTransformable (transformBy),
    LineSegment,
    Point,
    PointFunctor (pmap),
    PolyLine,
    SimplePolygon,
    Transformation,
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
  ( Expectation,
    Spec,
    describe,
    shouldBe,
    shouldSatisfy,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    arbitrarySizedFractional,
    choose,
  )

-- Convenience synonyms

type LS2F = LineSegment 2 Int FloatingNaNWrapper

type PL2F = PolyLine 2 Int FloatingNaNWrapper

type PGF = SimplePolygon Int FloatingNaNWrapper

type T2F = Transformation 2 FloatingNaNWrapper

type UIUR = UnitInterval UR

type LS2UR = LineSegment 2 Int UR

type PL2UR = PolyLine 2 Int UR

type PG2UR = SimplePolygon Int UR

type FGUR = FiniteGeometry Int UR

-- Wrappers

-- A wrapper around `Float` to change the behavior so that NaN == NaN
newtype FloatingNaNWrapper = FloatingNaNWrapper Float deriving (Arbitrary, Num, Fractional, Floating, Ord)

instance Eq FloatingNaNWrapper where
  (FloatingNaNWrapper a) == (FloatingNaNWrapper b) = (isNaN a && isNaN b) || (a == b)

instance Show FloatingNaNWrapper where
  show (FloatingNaNWrapper a) = show a

newtype FloatInUnitInterval = FloatInUnitInterval Float deriving (Eq, Num, Fractional, Ord)

instance Arbitrary FloatInUnitInterval where
  arbitrary = FloatInUnitInterval <$> choose (0, 1)

instance Show FloatInUnitInterval where
  show (FloatInUnitInterval x) = show x

-- A Rational drawn arbitrarily on the unit interval
newtype UR = UR Rational deriving (Eq, Num, Ord, Fractional, Show)

asDouble :: (Double -> Double) -> UR -> UR
asDouble f (UR x) = UR . toRational . f . fromRational $ x

instance Floating UR where
  pi = UR . toRational $ (pi :: Double)
  exp = asDouble exp
  log = asDouble log
  sin = asDouble sin
  cos = asDouble cos
  asin = asDouble asin
  acos = asDouble acos
  atan = asDouble atan
  sinh = asDouble sinh
  cosh = asDouble cosh
  asinh = asDouble sinh
  acosh = asDouble acosh
  atanh = asDouble atanh

instance Random UR where
  randomR (UR l, UR u) g = let (b, g') = randomR (fromRational l :: Double, fromRational u) g in (UR (toRational b), g')

  random g = randomR (0, 1) g

instance Arbitrary UR where
  arbitrary = UR <$> arbitrarySizedFractional

overUnits :: (Ord a, Fractional a) => (a -> a -> a) -> UnitInterval a -> UnitInterval a -> UnitInterval a
overUnits g x y = unitInterval (viewUnitInterval x `g` viewUnitInterval y)

divUnit :: (Ord a, Fractional a) => UnitInterval a -> UnitInterval a -> UnitInterval a
divUnit = overUnits (/)

subUnit :: (Ord a, Fractional a) => UnitInterval a -> UnitInterval a -> UnitInterval a
subUnit = overUnits (-)

newtype CloseEnough a = CloseEnough a deriving (Show)

-- Obviously this violates every law known to man, but it's just for tests.
instance Eq (CloseEnough UR) where
  (CloseEnough x) == (CloseEnough y) = abs (x - y) < 1e-8

instance Eq (CloseEnough (Point 2 UR)) where
  (CloseEnough p1) == (CloseEnough p2) = qdA p1 p2 < 1e-8

-- Test

pass :: Expectation
pass = 1 `shouldBe` 1

guard :: Bool -> Expectation -> Expectation
guard True e = pass
guard False e = e

shouldBeCloseEnough :: (Show a, Eq (CloseEnough a)) => a -> a -> Expectation
shouldBeCloseEnough x y = shouldBe (CloseEnough x) (CloseEnough y)

spec :: Spec
spec = do
  -- Basic typeclass tests
  describe "FiniteGeometry passes through boxes" $ do
    prop "with LineSegment" $
      \(x :: LS2F) -> boundingBox x `shouldBe` boundingBox (ATDLineSegment x)
    prop "with PolyLine" $
      \(x :: PL2F) -> boundingBox x `shouldBe` boundingBox (ATDPolyLine x)
    prop "with SimplePolygon" $
      \(x :: PGF) -> boundingBox x `shouldBe` boundingBox (ATDSimplePolygon x)
  describe "FiniteGeometry passes through transformations" $ do
    prop "with LineSegment" $
      \(x :: LS2F) (t :: T2F) -> (ATDLineSegment . transformBy t) x `shouldBe` transformBy t (ATDLineSegment x)
    prop "with PolyLine" $
      \(x :: PL2F) (t :: T2F) -> (ATDPolyLine . transformBy t) x `shouldBe` transformBy t (ATDPolyLine x)
    prop "with Polygon" $
      \(x :: PGF) (t :: T2F) -> (ATDSimplePolygon . transformBy t) x `shouldBe` transformBy t (ATDSimplePolygon x)
  describe "FiniteGeometry passes through pmap" $ do
    prop "with LineSegment" $
      \(x :: LS2F) (t :: T2F) -> (ATDLineSegment . pmap (transformBy t)) x `shouldBe` pmap (transformBy t) (ATDLineSegment x)
    prop "with PolyLine" $
      \(x :: PL2F) (t :: T2F) -> (ATDPolyLine . pmap (transformBy t)) x `shouldBe` pmap (transformBy t) (ATDPolyLine x)
    prop "with Polygon" $
      \(x :: PGF) (t :: T2F) -> (ATDSimplePolygon . pmap (transformBy t)) x `shouldBe` pmap (transformBy t) (ATDSimplePolygon x)
  describe "UnitInterval" $ do
    prop "clamps between 0 and 1" $
      \(x :: Float) -> (viewUnitInterval . unitInterval) x `shouldSatisfy` (\y -> y <= 1 && y >= 0)
    prop "otherwise does nothing" $
      \(x :: FloatInUnitInterval) -> (viewUnitInterval . unitInterval) x `shouldBe` x

  describe "Polygon interpolation" $ do
    prop "interpolants on line segment" $
      \(x :: UIUR) (ls :: FGUR) -> interp x ls `shouldSatisfy` (`intersects` ls)
    prop "interpolant ends first half" $
      \(x :: UIUR) (ls :: FGUR) ->
        interp (unitInterval 1) (fst (splitInterp x ls))
          `shouldBe` interp x ls
    prop "interpolant begins secnd  half" $
      \(x :: UIUR) (ls :: FGUR) ->
        interp (unitInterval 0) (snd (splitInterp x ls))
          `shouldBe` interp x ls
    prop "start of first half is start of whole" $
      \(x :: UIUR) (ls :: FGUR) ->
        interp (unitInterval 0) (fst (splitInterp x ls))
          `shouldBe` interp (unitInterval 0) ls
    prop "end of second half is end of whole" $
      \(x :: UIUR) (ls :: FGUR) ->
        interp (unitInterval 1) (snd (splitInterp x ls))
          `shouldBe` interp (unitInterval 1) ls
    prop "picking subportion of first works" $
      \(x :: UIUR) (y :: UIUR) (ls :: FGUR) ->
        let x' = min x y; y' = max x y
         in guard (y' == unitInterval 0) $
              interp x' ls
                `shouldBeCloseEnough` interp (divUnit x' y') (fst (splitInterp y' ls))
    prop "picking subportion of second works" $
      \(x :: UIUR) (y :: UIUR) (ls :: FGUR) ->
        let x' = min x y; y' = max x y
         in if x' == unitInterval 1
              then pass
              else
                interp y' ls
                  `shouldBeCloseEnough` interp (divUnit (subUnit y' x') (subUnit (unitInterval 1) x')) (snd (splitInterp x' ls))
    prop "first half has proportional length" $
      \(x :: UIUR) (ls :: FGUR) ->
        shapeLength (fst $ splitInterp x ls) `shouldBeCloseEnough` (viewUnitInterval x * shapeLength ls)
    prop "second half has proportional length" $
      \(x :: UIUR) (ls :: FGUR) ->
        shapeLength (snd $ splitInterp x ls) `shouldBeCloseEnough` ((1 - viewUnitInterval x) * shapeLength ls)
  describe "Projection Tests" $ do
    prop "interpolated points are their own projection" $
      \(x :: UIUR) (ls :: FGUR) ->
          let p = interp x ls
           in second (^. core) (project p ls)
                `shouldBe` (0, p)
    prop "projected points intersect shape" $ \(p :: Point 2 UR) (ls :: FGUR) ->
          let (_, p') = project p ls in (p'^.core) `shouldSatisfy` (`intersects` ls)