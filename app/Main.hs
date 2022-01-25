{-# LANGUAGE DataKinds #-}
module Main where

import Graphics.Color.Standard(SVG(..), color)
import Graphics.Color.Model (Color, Alpha, RGB)
import Graphics.Color.Space (SRGB, Linearity(NonLinear), Illuminant(..), unColorRGB, convertColor)
import Data.Word (Word16)
import Data.Vector.Fixed (Vector, Dim)
import Data.Vector.Fixed.Primitive (Vec)

import AtTheDisco.Layer(Layer)
import AtTheDisco.IO (saveRGBA16LayerPNG, saveRGB16LayerPNG)

white' = (color (SVG :: SVG "white") :: Color (SRGB 'NonLinear) Float)

white = unColorRGB white' :: Color RGB Float 

whiteImage :: Layer (Vec 2) Int (Color RGB Float)
whiteImage = const white

saveWhiteImage :: Int -> Int -> FilePath -> IO ()
saveWhiteImage = saveRGB16LayerPNG whiteImage

main :: IO ()
main = do
    saveWhiteImage 100 100 "samples/white.png"
    return ()