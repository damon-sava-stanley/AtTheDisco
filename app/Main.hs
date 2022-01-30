{-# LANGUAGE DataKinds #-}
module Main where

import Graphics.Color.Standard(SVG(..), color)
import Graphics.Color.Model (Color, Alpha, RGB)
import Graphics.Color.Space (SRGB, Linearity(NonLinear), Illuminant(..), unColorRGB, convertColor)
import Data.Word (Word16)
import Data.Geometry (Point(..), Vector(..))
import Data.Geometry.Vector.VectorFamily
import Data.Geometry.Box (Box)
import qualified Data.Geometry.Box as Box

import AtTheDisco.Layer(Layer, mask)
import AtTheDisco.Geometry.Shapes
import AtTheDisco.IO (saveRGBA16LayerPNG, saveRGB16LayerPNG)
import AtTheDisco.Color.Combination (blendLayers)


white = unColorRGB (color (SVG :: SVG "white") :: Color (SRGB 'NonLinear) Float) :: Color RGB Float

black = unColorRGB (color (SVG :: SVG "black") :: Color (SRGB 'NonLinear) Float) :: Color RGB Float

smallBox :: Box 2 () Float
smallBox = Box.fromCenter (Point2 50 50) (Vector2 40 40)

saveWhiteImage :: Int -> Int -> FilePath -> IO ()
saveWhiteImage = saveRGB16LayerPNG (const white)

saveBox = saveRGB16LayerPNG (mask (boxLayer smallBox) (const white) (const black))

saveLightGray = saveRGB16LayerPNG (blendLayers 0.7 (const white) (const black))

main :: IO ()
main = do
    saveWhiteImage 100 100 "samples/white.png"
    saveLightGray 100 100 "samples/blended-grey.png"
    saveBox 100 100 "samples/box.png"
    return ()