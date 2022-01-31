{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module AtTheDisco.IO
  ( saveRGBA16LayerPNG
  , saveRGB16LayerPNG
  , loadPNG16
  ) where

import           AtTheDisco.Layer     (Layer, unwrapVector)
import           AtTheDisco.Sample    (subsample)

import           Codec.Picture        (readPng)
import qualified Codec.Picture        as JP
import           Control.Lens         ((^.))
import           Data.Bits            (unsafeShiftR)
import           Data.Geometry        (Point, xCoord, yCoord)
import           Data.Word            (Word16, Word8)
import           Graphics.Color.Model (Alpha, Color, Elevator (..), RGB,
                                       fromComponents, toComponents)
import           System.IO            (FilePath)
import           Text.Printf          (FormatParse (fpChar))

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

layerToJPImage ::
     (JP.Pixel px, Num a)
  => Layer (Point 2) a px
  -> Int -- ^ Width
  -> Int -- ^ Height
  -> JP.Image px
layerToJPImage layer = JP.generateImage f
  where
    f = unwrapVector . subsample $ layer

saveRGBA16LayerPNG ::
     (Elevator e, Num a)
  => Layer (Point 2) a (Color (Alpha RGB) e)
  -> Int
  -> Int
  -> FilePath
  -> IO ()
saveRGBA16LayerPNG l w h fp =
  JP.writePng fp $ layerToJPImage (colorToJPRGBA16 . fmap toWord16 . l) w h

saveRGB16LayerPNG ::
     (Elevator e, Num a)
  => Layer (Point 2) a (Color RGB e)
  -> Int
  -> Int
  -> FilePath
  -> IO ()
saveRGB16LayerPNG l w h fp =
  JP.writePng fp $ layerToJPImage (colorToJPRGB16 . fmap toWord16 . l) w h

-- | Load a PNG and convert it to RGB 16.
loadPNG16 ::
     FilePath
  -> Color RGB Word16
  -> IO (Either String (Layer (Point 2) Int (Color RGB Word16)))
loadPNG16 fp def = do
  image'' <- JP.readPng fp
  case image'' of
    Left s -> return (Left s)
    Right image' ->
      let image = JP.convertRGB16 image'
          width = JP.imageWidth image
          height = JP.imageHeight image
          pix x y
            | x < 0 || x >= width = def
            | y < 0 || y >= height = def
            | otherwise = jpRGB16ToColor $ JP.pixelAt image x y
       in return . Right $ (\v -> pix (v ^. xCoord) (v ^. yCoord))
