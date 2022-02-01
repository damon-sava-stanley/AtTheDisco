{-# LANGUAGE DataKinds #-}

module Main where

import           AtTheDisco.Color.Combination      (blendLayers)
import           AtTheDisco.Color.Gradient         (colorGradientValue,
                                                    newGradient)
import           AtTheDisco.Geometry.Gradient      (lineSegmentGradient)
import           AtTheDisco.Geometry.Shapes        (Drawable (draw))
import           AtTheDisco.Geometry.Translations
import           AtTheDisco.IO                     (loadPNG16,
                                                    saveRGB16LayerPNG,
                                                    saveRGBA16LayerPNG)
import           AtTheDisco.Layer                  (Layer, ScreenLayer, mask)
import           Control.Lens                      ((^.))
import           Data.Either                       (fromRight)
import           Data.Geometry                     (Point (..), Vector (..),
                                                    origin, xCoord, yCoord)
import           Data.Geometry.Box                 (Box)
import qualified Data.Geometry.Box                 as Box
import           Data.Geometry.Vector.VectorFamily
import           Data.Word                         (Word16)
import           Graphics.Color.Model              (Alpha, Color, Elevator (toDouble, toWord16),
                                                    RGB)
import           Graphics.Color.Space              (Illuminant (..),
                                                    Linearity (NonLinear), SRGB,
                                                    convertColor, unColorRGB)
import           Graphics.Color.Standard           (SVG (..), color)

white =
  unColorRGB (color (SVG :: SVG "white") :: Color (SRGB 'NonLinear) Double) :: Color RGB Double

black =
  unColorRGB (color (SVG :: SVG "black") :: Color (SRGB 'NonLinear) Double) :: Color RGB Double

smallBox :: Box 2 () Double
smallBox = Box.fromCenter (Point2 50 50) (Vector2 40 40)

saveWhiteImage :: Int -> Int -> FilePath -> IO ()
saveWhiteImage = saveRGB16LayerPNG (const white)

saveBox = saveRGB16LayerPNG (mask (draw smallBox) (const white) (const black))

saveLightGray = saveRGB16LayerPNG (blendLayers 0.7 (const white) (const black))

saveGradient =
  saveRGB16LayerPNG
    (\p -> colorGradientValue ((p ^. xCoord) / 100) $ newGradient black white)

saveDiagonalGradient =
  saveRGB16LayerPNG
    (\p ->
       colorGradientValue (lineSegmentGradient (Point2 20 10) (Vector2 10 70) p) $
       newGradient black white)

blendBothGradients :: IO (Layer (Point 2) Int (Color RGB Double))
blendBothGradients =
  let grad1 :: IO (Layer (Point 2) Int (Color RGB Double))
      grad1 = do
        x <- loadPNG16 "samples/gradient.png" (toWord16 <$> black)
        let y =
              (fromRight (const (toWord16 <$> black)) x :: Layer (Point 2) Int (Color RGB Word16))
        return $ fmap toDouble . y
      grad2 :: IO (Layer (Point 2) Int (Color RGB Double))
      grad2 = do
        x <- loadPNG16 "samples/diagonalGradient.png" (toWord16 <$> black)
        let y = fromRight (const (toWord16 <$> black)) x
        return $ fmap toDouble . y
   in blendLayers 0.5 <$> grad1 <*> grad2

checkers :: (RealFrac a) => ScreenLayer (Point 2) a
checkers p = odd (floor (p ^. xCoord) + floor (p ^. yCoord))

coloredCheckers :: (RealFrac a) => Layer (Point 2) a (Color RGB Double)
coloredCheckers = mask checkers (const white) (const black)

scaledCheckers :: (Floating a, RealFrac a) => ScreenLayer (Point 2) a
scaledCheckers = scaleLayerAbout origin (Vector2 2 2) checkers

polarCheckers ::
     (RealFloat a, Floating a, RealFrac a) => ScreenLayer (Point 2) a
polarCheckers =
  translateLayer (Vector2 50 50) .
  toPolarLayer . scaleLayerAbout origin (Vector2 1 (1 / pi)) $
  scaledCheckers

coloredPolarCheckers ::
     (RealFloat a, Floating a, RealFrac a)
  => Layer (Point 2) a (Color RGB Double)
coloredPolarCheckers = mask polarCheckers (const white) (const black)

main :: IO ()
main = do
  saveWhiteImage 100 100 "samples/white.png"
  saveLightGray 100 100 "samples/blended-grey.png"
  saveBox 100 100 "samples/box.png"
  saveGradient 100 100 "samples/gradient.png"
  saveDiagonalGradient 100 100 "samples/diagonalGradient.png"
  gradients <- blendBothGradients
  saveRGB16LayerPNG gradients 100 100 "samples/blendedGradients.png"
  saveRGB16LayerPNG coloredCheckers 100 100 "samples/coloredCheckers.png"
  saveRGB16LayerPNG
    coloredPolarCheckers
    100
    100
    "samples/coloredPolarCheckers.png"
  return ()
