{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.AtTheDisco.GeometryTypes where


import AtTheDisco.Geometry
    ( unitInterval, viewUnitInterval, FiniteGeometry, UnitInterval )
import Data.Geometry
    ( Point,
      LineSegment,
      PolyLine,
      SimplePolygon,
      Transformation,
      qdA )
import System.Random ( Random(randomR, random) ) 
import Test.QuickCheck
    ( Arbitrary(arbitrary), arbitrarySizedFractional, choose )
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
newtype FloatingNaNWrapper = FloatingNaNWrapper Float deriving (Arbitrary, Num, Fractional, Floating, Ord, Random)

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

  random g = let (a :: Double, g') = random g in (UR (toRational a), g)

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