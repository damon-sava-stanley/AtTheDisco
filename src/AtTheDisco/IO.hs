{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module AtTheDisco.IO where

import AtTheDisco.Layer
import Codec.Picture (BmpEncodable, Pixel, generateImage, writeBitmap, Image)
import Control.Lens ((^.))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Data (Proxy)
import Data.Ext (core)
import Data.Geometry (Dimension, NumType, Point (Point2), Vector, vector, (.+^))
import Data.Geometry.Box (IsBoxable (boundingBox), height, minPoint, size, width)
import Data.Geometry.Vector.VectorFamily

-- | Given a layer, return a 'Vector' representing its dimensions a coloring function that starts at '(0, 0)' and goes up to those dimensions.
drawPicture :: Integral a => Layer Total HasBoundingBox a c -> (Vector 2 a, Point 2 a -> c)
drawPicture layer = (dims, \p -> drawTotal layer (p .+^ o))
  where
    box = layerBoundingBox layer
    dims = size box
    o = minPoint box ^. core . vector

writePictureBMP :: (Pixel c, BmpEncodable c)
                => Layer Total HasBoundingBox Int c -> String -> IO ()
writePictureBMP shape fp = writeBitmap fp image
  where
    (Vector2 w h, gen) = drawPicture shape
    image = generateImage (\x y -> gen (Point2 x y)) w h

