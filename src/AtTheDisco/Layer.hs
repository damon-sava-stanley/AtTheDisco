{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module : AtTheDisco.Geometry
-- Description : Functionality to draw geometry.
-- Copyright : (c) Damon Stanley, 2022
-- License : BSD3
-- Maintainer : damonsava@gmail.com
-- Stability : experimental
--
-- /Layers/ are the abstraction used to organize drawings and style geometry.
module AtTheDisco.Layer(
  -- * Layers
  -- $layers
  BoundingBox(..),
  Partiality(..),
  Layer(..),
  DumbLayer(..),
  -- * Drawing
  -- $drawLayers
  layerBoundingBox,
  tryLayerBoundingBox,
  drawTotal,
  drawPartially
) where

import AtTheDisco.Geometry
  ( FiniteGeometries,
    HasInside (isInside),
    Projectable (project),
  )
import Data.Geometry (Point)
import Data.Maybe (fromJust, mapMaybe, catMaybes)
import Data.Geometry.Box
import qualified Data.List.NonEmpty as NE
import Data.Geometry.Point (PointFunctor(pmap))

-- $layers
--
-- Definition of 'Layer', the main type.


-- data Partiality = Partial | Total
data BoundingBox = HasBoundingBox | LacksBoundingBox

data Partiality where
  Partial :: Partiality
  Total :: Partiality

type AtopPartiality :: Partiality -> Partiality -> Partiality
type family AtopPartiality t b where
  AtopPartiality Partial Partial = Partial
  AtopPartiality Total Partial = Total
  AtopPartiality Partial Total = Total
  AtopPartiality Total Total = Total

type AtopBoundingBox :: BoundingBox -> BoundingBox -> BoundingBox
type family AtopBoundingBox t b where
  AtopBoundingBox HasBoundingBox HasBoundingBox = HasBoundingBox
  AtopBoundingBox LacksBoundingBox HasBoundingBox = HasBoundingBox
  AtopBoundingBox HasBoundingBox LacksBoundingBox = HasBoundingBox
  AtopBoundingBox LacksBoundingBox LacksBoundingBox = HasBoundingBox

type DemotePartiality :: Partiality -> *
type family DemotePartiality p where
  DemotePartiality Partial = Partiality

-- | A 'Layer' is a structured representation of a continuous, partially defined image. We keep track of 
--  whether that image has a bounding box and whether it is everywhere defined or not.
data Layer :: Partiality -> BoundingBox -> * -> * -> *  where
  -- | A 'NullLayer' is an empty layer.
  NullLayer :: Layer Partial LacksBoundingBox r c
  -- | A 'ConstantLayer' is a total layer that returns a constant layer.
  ConstantLayer :: c -> Layer Total LacksBoundingBox r c
  -- | A 'GeometryLayer' wraps an underlying 'FiniteGeometries' with styling information.
  GeometryLayer :: (Fractional r, Ord r) => FiniteGeometries x r -> r -> Maybe c -> Layer p b r c -> Layer Partial HasBoundingBox r c
  -- | A 'FillHolesLayer' fills holes in the first layer with holes in the previous layers.
  --   The bounding box will be resized to fit the bounding boxes of the previous.
  FillHolesLayer :: (Ord r) => Layer p1 b1 r c -> Layer p2 b2 r c -> Layer (AtopPartiality p1 p2)(AtopBoundingBox b1 b2) r c
  -- | A 'SampleLayer' samples the underlying layer.
  SampleLayer :: (RealFrac r, Integral s) => Layer p b r c -> Layer p b s c

-- | A 'DumbLayer' is a 'Layer' where we have thrown away 'Partiality' and 'BoundingBox' information.
data DumbLayer r c = forall p b . DumbLayer (Layer p b r c)

instance Ord r => Semigroup (DumbLayer r c) where
  (DumbLayer t) <> (DumbLayer b) = DumbLayer (FillHolesLayer t b)

instance Ord r => Monoid (DumbLayer r c) where
  mempty = DumbLayer NullLayer

-- $drawLayers
--
-- Functions for drawing layers.

-- Helper
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust _ m = m

-- | Get the bounding box when we can.
layerBoundingBox :: Layer p HasBoundingBox r c -> Rectangle () r
layerBoundingBox = fromJust . tryLayerBoundingBox -- Need to make sure we keep Layer's promises.

-- | Get the layer bounding box if we can. Will succeed when layer 'HasBoundingBox' and fail when it 'LacksBoundingBox'
tryLayerBoundingBox :: Layer p b r c -> Maybe (Rectangle () r)
tryLayerBoundingBox NullLayer = Nothing
tryLayerBoundingBox ConstantLayer {} = Nothing
tryLayerBoundingBox (GeometryLayer g _ _ _) = Just $ boundingBox g
tryLayerBoundingBox (FillHolesLayer l p) = let b1 = tryLayerBoundingBox l
                                               b2 = tryLayerBoundingBox p
                                               boxes = catMaybes [b1, b2]
                                            in boundingBoxList <$> NE.nonEmpty boxes
tryLayerBoundingBox (SampleLayer l) = pmap (fmap round) <$> tryLayerBoundingBox l

-- | Get the bounding box of a 'DumbLayer'. Must return 'Maybe' as we have forgotten whether it has one.
layerBoundingBoxDumb :: DumbLayer r c -> Maybe (Rectangle () r)
layerBoundingBoxDumb (DumbLayer l) = tryLayerBoundingBox l

-- | Draw a total layer.
drawTotal :: Layer Total b r c -> Point 2 r -> c
drawTotal l = fromJust . drawPartially l -- N.B. need to be very careful that this is safe.

-- | Draw a layer as if it were partial.
drawPartially :: Layer p b r c -> Point 2 r -> Maybe c
drawPartially NullLayer p = Nothing
drawPartially (ConstantLayer c) p = Just c
drawPartially (GeometryLayer geometry width lineColor fill) point =
  let (r, _) = project point geometry
      lc = if 2 * r <= width * width then lineColor else Nothing
      fc = drawPartially fill point
   in lc `firstJust` fc
drawPartially (FillHolesLayer t b) point = drawPartially t point `firstJust` drawPartially b point
drawPartially (SampleLayer l) point = drawPartially l (fmap fromIntegral point)

-- | Draw a 'DumbLayer', since we don't know whether it's partial, we have to assume that it is.
drawDumb :: DumbLayer r c -> Point 2 r -> Maybe c
drawDumb (DumbLayer l) = drawPartially l