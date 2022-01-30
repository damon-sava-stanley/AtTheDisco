{-# LANGUAGE DataKinds #-}
module Main where

import Graphics.Color.Standard(SVG(..), color)
import Graphics.Color.Model (Color, Alpha, RGB)
import Graphics.Color.Space (SRGB, Linearity(NonLinear), Illuminant(..), unColorRGB, convertColor)
import Data.Word (Word16)
import Data.Geometry (Point(..), Vector(..), xCoord)
import Data.Geometry.Vector.VectorFamily
import Data.Geometry.Box (Box)
import qualified Data.Geometry.Box as Box
import Control.Lens((^.))
import AtTheDisco.Layer(Layer, mask)
import AtTheDisco.Geometry.Shapes ( boxLayer )
import AtTheDisco.IO (saveRGBA16LayerPNG, saveRGB16LayerPNG)
import AtTheDisco.Color.Combination (blendLayers)
import AtTheDisco.Color.Gradient (colorGradientValue, newGradient)
import AtTheDisco.Geometry.Gradient (lineSegmentGradient)


white = unColorRGB (color (SVG :: SVG "white") :: Color (SRGB 'NonLinear) Double) :: Color RGB Double

black = unColorRGB (color (SVG :: SVG "black") :: Color (SRGB 'NonLinear) Double) :: Color RGB Double

smallBox :: Box 2 () Double
smallBox = Box.fromCenter (Point2 50 50) (Vector2 40 40)

saveWhiteImage :: Int -> Int -> FilePath -> IO ()
saveWhiteImage = saveRGB16LayerPNG (const white)

saveBox = saveRGB16LayerPNG (mask (boxLayer smallBox) (const white) (const black))

saveLightGray = saveRGB16LayerPNG (blendLayers 0.7 (const white) (const black))

saveGradient = saveRGB16LayerPNG (\p -> colorGradientValue ((p^.xCoord) / 100) $ newGradient black white)

saveDiagonalGradient = saveRGB16LayerPNG (\p -> colorGradientValue (lineSegmentGradient (Point2 20 10) (Vector2 10 70) p) $ newGradient black white)

main :: IO ()
main = do
    saveWhiteImage 100 100 "samples/white.png"
    saveLightGray 100 100 "samples/blended-grey.png"
    saveBox 100 100 "samples/box.png"
    saveGradient 100 100 "samples/gradient.png"
    saveDiagonalGradient 100 100 "samples/diagonalGradient.png"
    return ()