{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

module Test.AtTheDisco.LayerSpec where

import AtTheDisco.Geometry
import AtTheDisco.Layer
import Control.Lens
import Data.Bifunctor (Bifunctor (first))
import Data.Geometry
import Test.AtTheDisco.ArbitraryGeometry
import Test.AtTheDisco.ArbitraryLayer
import Test.AtTheDisco.GeometryTypes
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import qualified Data.LSeq as LSeq
import Data.Data ( Proxy(..) )
import AtTheDisco.Builder
import Data.Maybe (isNothing)

data GeometryStyle c r = GeometryStyle r c c deriving (Show)

instance Arbitrary2 GeometryStyle where
  liftArbitrary2 genC genR = GeometryStyle <$> genR <*> genC <*> genC

instance Arbitrary r => Arbitrary1 (GeometryStyle r) where
  liftArbitrary = liftArbitrary2 arbitrary

instance (Arbitrary r, Arbitrary c) => Arbitrary (GeometryStyle r c) where
  arbitrary = liftArbitrary arbitrary

drawPoly :: (Fractional r, Ord r) => SimplePolygon () r -> GeometryStyle c r -> Point 2 r -> Maybe c 
drawPoly pg = drawGeometry (fgSingle (ATDSimplePolygon pg))

drawGeometry :: (Fractional r, Ord r) => FiniteGeometries () r -> GeometryStyle c r -> Point 2 r -> Maybe c
drawGeometry geo (GeometryStyle t l f) = drawPartially (GeometryLayer geo t (Just l) (ConstantLayer f))

spec :: Spec
spec = do
  describe "Drawing Geometry" $ do
    prop "points on boundary drawn" $ do
      \(c :: Int) (x :: UIUR) (sh :: SimplePolygon () UR) ->
        drawPoly sh (GeometryStyle 0.1 c (c+1)) (interp x sh)
          `shouldBe` Just c
    prop "inside of a polygon drawn" $ do
      \(c :: Int) (PolygonAroundPoint p pg :: PolygonAroundPoint () UR) ->
        drawPoly pg (GeometryStyle 0 (c+1) c) p `shouldBe` Just c
  describe "Layer Tests" $ do
    prop "satisfies semigroup associativity" $
      \(a :: DumbLayer UR Int) b c p -> 
        drawDumb ((a<>b)<>c) p `shouldBe` drawDumb (a<>(b<>c)) p
    prop "mempty drops out left" $
      \(a :: DumbLayer UR Int) p ->
        drawDumb (mempty <> a) p `shouldBe` drawDumb a p
    prop "mempty drops out right" $
      \(a :: DumbLayer UR Int) p ->
        drawDumb (a <> mempty)  p `shouldBe` drawDumb a p
  describe "Drawing Tests" $ do
    prop "NullLayer always nothing" $ do
      \(p :: Point 2 Int) ->
        (drawPartially NullLayer p :: Maybe ()) `shouldSatisfy` isNothing