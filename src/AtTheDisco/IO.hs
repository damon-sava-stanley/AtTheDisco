{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module AtTheDisco.IO (saveRGBA16LayerPNG, saveRGB16LayerPNG) where

import AtTheDisco.Layer(Layer, unwrapVector)
import AtTheDisco.Sample(subsample)

import qualified Codec.Picture as JP
import Graphics.Color.Model (Color, Alpha, RGB, Elevator(..), toComponents, fromComponents)
import Data.Word (Word16, Word8)
import Data.Vector.Fixed (Vector, Dim)
import System.IO (FilePath)

-- TODO: Some TypeClass stuff to get more types on board and have a nicer
-- interface

-- Color <=> JuicyPixel conversions

colorToJPRGBA16 :: Color (Alpha RGB) Word16 -> JP.PixelRGBA16
colorToJPRGBA16 c = JP.PixelRGBA16 r g b a
    where
        ((r, g, b), a) = toComponents c

jpRGBA16ToColor :: JP.PixelRGBA16 -> Color (Alpha RGB) Word16
jpRGBA16ToColor (JP.PixelRGBA16 r g b a) = fromComponents ((r, g, b), a)

colorToJPRGB16 :: Color RGB Word16 -> JP.PixelRGB16
colorToJPRGB16 c = JP.PixelRGB16 r g b
    where
        (r, g, b) = toComponents c

jpRGB16ToColor :: JP.PixelRGB16 -> Color RGB Word16
jpRGB16ToColor (JP.PixelRGB16 r g b) = fromComponents (r, g, b)

layerToJPImage :: (JP.Pixel px, Num a, Vector v a, Vector v Int, Dim v ~ 2) 
               => Layer v a px 
               -> Int -- ^ Width
               -> Int -- ^ Height 
               -> JP.Image px
layerToJPImage layer w h = JP.generateImage f w h 
    where
        f = unwrapVector . subsample $ layer

saveRGBA16LayerPNG :: (Elevator e, Num a, Vector v a, Vector v Int, Dim v ~ 2)
                   => Layer v a (Color (Alpha RGB) e)
                   -> Int
                   -> Int
                   -> FilePath
                   -> IO ()
saveRGBA16LayerPNG l w h fp = JP.writePng fp $ layerToJPImage (colorToJPRGBA16 . fmap toWord16 . l) w h

saveRGB16LayerPNG :: (Elevator e, Num a, Vector v a, Vector v Int, Dim v ~ 2)
                  => Layer v a (Color RGB e)
                  -> Int
                  -> Int
                  -> FilePath
                  -> IO ()
saveRGB16LayerPNG l w h fp = JP.writePng fp $ layerToJPImage (colorToJPRGB16 . fmap toWord16 . l) w h