{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module AtTheDisco.IO where

import AtTheDisco.Layer (Drawable (draw))
import Codec.Picture (BmpEncodable, Pixel, generateImage, writeBitmap, Image)
import Control.Lens ((^.))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Data (Proxy)
import Data.Ext (core)
import Data.Geometry (Dimension, NumType, Point (Point2), Vector, vector, (.+^))
import Data.Geometry.Box (IsBoxable (boundingBox), height, minPoint, size, width)
import Data.Geometry.Vector.VectorFamily

-- | Given a `Drawable` and `IsBoxable` 2d shape, return a vector representing its dimensions (rounded down) and a
--   drawing function that starts from (0, 0)
drawPicture :: (2 ~ Dimension (f r), r ~ NumType (f r), 
                Ord r, RealFrac r, Drawable f r c, IsBoxable (f r)) 
            => f r -> (Vector 2 Int, Point 2 Int -> c)
drawPicture shape = (dims, \p -> draw shape $ fmap fromIntegral p .+^ o)
  where
    box = boundingBox shape
    dims = floor <$> size box
    o = minPoint box ^. core . vector

writePictureBMP :: forall f r c . (2 ~ Dimension (f r), r ~ NumType (f r), 
                    Ord r, RealFrac r, 
                    Drawable f r c, IsBoxable (f r), 
                    Pixel c, BmpEncodable c) 
                => Proxy c -> f r -> String -> IO ()
writePictureBMP _ shape fp = writeBitmap fp image
  where
    (Vector2 w h, gen) = drawPicture shape
    image :: Image c -- NB: type signature needed to help GHC out.
    image = generateImage (\x y -> gen (Point2 x y)) w h