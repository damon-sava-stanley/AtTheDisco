{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Test.AtTheDisco.ArbitraryLayer where
import Test.QuickCheck
import System.Random (Random)
import AtTheDisco.Layer
import AtTheDisco.Geometry (FiniteGeometries)
import Test.AtTheDisco.ArbitraryGeometry

instance forall r c . (Arbitrary c, Arbitrary r, Floating r, Random r, Ord r) 
                    => Arbitrary (DumbLayer r c) where
  arbitrary = do
    n <- getSize 
    s <- choose (0, n)
    let ns = max (s - 1) 0
    case s of 
      0 -> do
        flatLayerType :: Int <- choose (0, 2)
        case flatLayerType of
          0 -> return (DumbLayer NullLayer)
          1 -> DumbLayer . ConstantLayer <$> arbitrary
          _ -> do
            geos :: FiniteGeometries () r <- arbitrary
            thickness :: r <- abs <$> arbitrary
            lineColor :: Maybe c <- arbitrary 
            DumbLayer fill <- resize ns arbitrary :: Gen (DumbLayer r c)
            return . DumbLayer $ GeometryLayer geos thickness lineColor fill
      _ -> do
        DumbLayer top <- resize ns arbitrary :: Gen (DumbLayer r c)
        DumbLayer bot <- resize ns arbitrary :: Gen (DumbLayer r c)
        return . DumbLayer $ FillHolesLayer top bot 
  shrink (DumbLayer (FillHolesLayer t b)) = [DumbLayer t, DumbLayer b]
  shrink (DumbLayer (GeometryLayer geo t lc f)) = 
    let shrunkGeo = fmap (\g -> DumbLayer $ GeometryLayer g t lc f) . shrink $ geo
    in DumbLayer f : shrunkGeo
  shrink _ = []