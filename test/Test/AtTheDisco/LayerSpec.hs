{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.AtTheDisco.LayerSpec where

import AtTheDisco.Geometry
import AtTheDisco.Layer
import Control.Lens
import Data.Bifunctor (Bifunctor (first))
import Data.Geometry
import Test.AtTheDisco.ArbitraryGeometry
import Test.AtTheDisco.GeometryTypes
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import qualified Data.LSeq as LSeq
import Data.Data ( Proxy(..) )

instance Arbitrary2 GeometryStyle where
  liftArbitrary2 genC genR = do
    (thickness :: r) <- genR
    (color :: Maybe c) <- liftArbitrary genC
    (fillColor :: Maybe c) <- liftArbitrary genC
    let fill = fmap (const . Just) fillColor
    let gs :: GeometryStyle c r = GeometryStyle thickness color fill
    return gs

instance Arbitrary r => Arbitrary1 (GeometryStyle r) where
  liftArbitrary = liftArbitrary2 arbitrary

instance (Arbitrary r, Arbitrary c) => Arbitrary (GeometryStyle r c) where
  arbitrary = liftArbitrary arbitrary

spec :: Spec
spec = do
  describe "Drawing Geometry" $ do
    prop "points on boundary drawn" $ do
      \(c :: Int) (s :: GeometryStyle Int UR) (x :: UIUR) (sh :: FGUR) ->
        draw (first (const $s & geometryLineColor ?~ c) sh) (interp x sh)
          `shouldBe` Just c
    prop "inside of a polygon drawn" $ do
      \(c :: Int) (PolygonAroundPoint p pg :: PolygonAroundPoint Int UR) ->
        let pg' = first (const $ GeometryStyle (0 :: UR) Nothing (Just . const $ Just c)) pg
         in draw pg' p `shouldBe` Just c
    prop "first polygon wins" $ do
      \(c :: Int) (PolygonAroundPoint p pg :: PolygonAroundPoint Int UR) ->
        let pg' = first (const $ GeometryStyle (0 :: UR) Nothing (Just . const $ Just c)) pg
            pg'' = first (const $ GeometryStyle (0 :: UR) Nothing (Just . const $ Just (c + 1))) pg
            fgs = FiniteGeometries (LSeq.forceLSeq (Proxy :: Proxy 1) . LSeq.fromList $
                    [ATDSimplePolygon pg', ATDSimplePolygon pg''])
         in draw fgs p `shouldBe` Just c