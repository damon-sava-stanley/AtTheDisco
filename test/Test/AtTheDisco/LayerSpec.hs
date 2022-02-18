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
import AtTheDisco.Builder

data GeometryStyle c r = GeometryStyle r c c deriving (Show)

instance Arbitrary2 GeometryStyle where
  liftArbitrary2 genC genR = GeometryStyle <$> genR <*> genC <*> genC

instance Arbitrary r => Arbitrary1 (GeometryStyle r) where
  liftArbitrary = liftArbitrary2 arbitrary

instance (Arbitrary r, Arbitrary c) => Arbitrary (GeometryStyle r c) where
  arbitrary = liftArbitrary arbitrary

drawPoly :: (Fractional r, Ord r) => SimplePolygon x r -> GeometryStyle c r -> Point 2 r -> Maybe c 
drawPoly pg = drawGeometry (fgSingle (ATDSimplePolygon pg))

drawGeometry :: (Fractional r, Ord r) => FiniteGeometries x r -> GeometryStyle c r -> Point 2 r -> Maybe c
drawGeometry geo (GeometryStyle t l f) = drawPartially (GeometryLayer geo t (Just l) (ConstantLayer f))

spec :: Spec
spec = do
  describe "Drawing Geometry" $ do
    prop "points on boundary drawn" $ do
      \(c :: Int) (x :: UIUR) (sh :: PG2UR) ->
        drawPoly sh (GeometryStyle 0.1 c (c+1)) (interp x sh)
          `shouldBe` Just c
    prop "inside of a polygon drawn" $ do
      \(c :: Int) (PolygonAroundPoint p pg :: PolygonAroundPoint Int UR) ->
        drawPoly pg (GeometryStyle 0 (c+1) c) p `shouldBe` Just c