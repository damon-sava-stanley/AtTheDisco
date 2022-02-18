{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.AtTheDisco.GeometrySpec (spec) where

import AtTheDisco.Geometry
import Data.Bifunctor ( Bifunctor(second) )
import Control.Lens ((^.))
import Data.Ext (core,type  (:+) ((:+)))
import Data.Foldable (Foldable (toList))
import Data.Geometry
  ( HasIntersectionWith (intersects),
    IsTransformable (transformBy),
    LineSegment,
    Point (Point2),
    PointFunctor (pmap),
    PolyLine,
    SimplePolygon,
    Transformation,
    qdA, pickPoint, insidePolygon
  )
import Data.Geometry.Box (IsBoxable (boundingBox))
import Data.Intersection (HasIntersectionWith (intersects))
import Test.AtTheDisco.ArbitraryGeometry (PolygonAroundPoint (PolygonAroundPoint))
import Test.AtTheDisco.GeometryTypes
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    shouldBe,
    shouldSatisfy, context, it
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.Maybe (isJust)
import Data.Geometry.Polygon (fromPoints)
import qualified Data.LSeq as LSeq 

-- Test

pass :: Expectation
pass = 1 `shouldBe` 1

failTest :: Expectation
failTest = 0 `shouldBe` 1

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
         in project p ls `shouldBe` (0, p)
    prop "projected points intersect shape" $ \(p :: Point 2 UR) (ls :: FGUR) ->
      let (_, p') = project p ls in p'  `shouldSatisfy` (`intersects` ls)
  describe "Inside Tests" $ do
    it "inside polygon sanity check"   
      (let p :: Point 2 UR = Point2 0 0
           pg :: SimplePolygon Int UR = fromPoints [Point2 1000000 0 :+ 0, 
             Point2 (-500000.06) 866025.4 :+ 0, Point2 (-499999.9) (-866025.44) :+ 0]
      in p `shouldSatisfy` (`insidePolygon` pg))
    it "getinside polygon sanity check"   
      (let p :: Point 2 UR = Point2 0 0
           pg :: SimplePolygon Int UR = fromPoints [Point2 1000000.0 0.0 :+ 0,
             Point2 (-500000.06) 866025.4 :+ 0, Point2 (-499999.9) (-866025.44) :+ 0]
      in p `shouldSatisfy` (`isInside` pg))
    prop "inside a polygon is inside" $ do
      \(PolygonAroundPoint p pg :: PolygonAroundPoint Int UR) -> p `shouldSatisfy` (`isInside` pg)