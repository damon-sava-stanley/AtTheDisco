{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module AtTheDisco.IO where

import AtTheDisco.Layer
import Codec.Picture (BmpEncodable, Pixel, generateImage, writeBitmap, Image, PixelRGB8 (PixelRGB8))
import Control.Lens ((^.))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Data (Proxy)
import Data.Ext (core)
import Data.Geometry (Dimension, NumType, Point (Point2), Vector, vector, (.+^), origin)
import Data.Geometry.Box (IsBoxable (boundingBox), height, minPoint, size, width)
import Data.Geometry.Vector.VectorFamily
import Data.Maybe (fromMaybe)



-- | Given a layer, return a 'Vector' representing its dimensions a coloring function that starts at '(0, 0)' and goes up to those dimensions.
drawPicture :: Integral a => Layer Total HasBoundingBox a c -> (Vector 2 a, Point 2 a -> c)
drawPicture layer = (dims, \p -> drawTotal layer (p .+^ o))
  where
    box = layerBoundingBox layer
    dims = size box
    o = minPoint box ^. core . vector

drawDumbPicture :: Integral a => DumbLayer a c -> (Maybe (Vector 2 a) , Point 2 a -> Maybe c)
drawDumbPicture layer = (dims, \p -> drawDumb layer (p .+^ fromMaybe (Vector2 0 0) o))
  where
    box = layerBoundingBoxDumb layer
    dims = size <$> box
    o = (^. core . vector) . minPoint <$> box


writeBMPInner (Vector2 w h, gen) fp = writeBitmap fp image
  where
    image = generateImage (\x y -> gen (Point2 x y)) w h

writePictureBMP :: (Pixel c, BmpEncodable c)
                => Layer Total HasBoundingBox Int c -> String -> IO ()
writePictureBMP shape = writeBMPInner (drawPicture shape)


data WritingResult = Fine | NoBoundingBox

missingColor :: PixelRGB8
missingColor = PixelRGB8 255 0 255

writeDumb8BitBMP :: DumbLayer Int PixelRGB8 -> String -> IO WritingResult
writeDumb8BitBMP shape fp = case drawDumbPicture shape
  of (Nothing, _) -> return NoBoundingBox
     (Just s, f) -> do
       writeBMPInner (s, fromMaybe missingColor . f) fp
       return Fine
