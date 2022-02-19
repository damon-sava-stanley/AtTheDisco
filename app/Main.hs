{-# LANGUAGE DataKinds #-}

module Main where
import AtTheDisco.Examples.ScatteredLines
import AtTheDisco.Layer (layerBoundingBoxDumb, drawDumb, DumbLayer)
import Data.Geometry (Point(Point2))
import AtTheDisco.Builder 
import Data.Functor.Identity (Identity(runIdentity))
import System.Random (mkStdGen)
import Codec.Picture
import AtTheDisco.IO

pureBlack :: DumbLayer Int PixelRGB8
pureBlack = snd . runIdentity . flip runMonadAssemblyT (mkStdGen 42) $ do
  crop (0, 0) (100, 100) (solidColor (PixelRGB8 0 0 0))  

main :: IO ()
main = do
  let s = cefaultScatterLines
  --print (layerBoundingBoxDumb s)
  --print (drawDumb s (Point2 500 500))
  saveScatteredLines "samples/scattered-lines.bmp"
  writeDumb8BitBMP pureBlack "samples/black.bmp"
  return ()