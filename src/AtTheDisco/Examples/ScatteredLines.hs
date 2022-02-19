{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AtTheDisco.Examples.ScatteredLines where

import AtTheDisco.Builder
import AtTheDisco.IO (WritingResult, writeDumb8BitBMP)
import AtTheDisco.Layer (DumbLayer (DumbLayer), Layer (SampleLayer))
import Codec.Picture (PixelRGB8 (PixelRGB8))
import Control.Lens ((&), (.~), (?~))
import Control.Monad (replicateM_)
import Control.Monad.Random (mkStdGen)
import Data.Bifunctor (Bifunctor (first))
import Data.Default (Default (def))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Ratio (denominator, (%))
import System.Random (Random (..))

black :: PixelRGB8
black = PixelRGB8 0 0 0

white :: PixelRGB8
white = PixelRGB8 255 255 255

instance Random Rational where
  random g =
    let (numerator :: Integer, g1) = random g
        (denom :: Integer, g2) = first ((+ 1) . abs) . random $ g1
     in (numerator % denom, g2)
  randomR (lb', ub') g =
    let (lb, ub) = (min lb' ub', max lb' ub')
        (denom :: Integer, g1) = first ((+ 1) . abs) . random $ g
        (num :: Integer, g2) = randomR (ceiling (denom % 1 * lb), floor (denom % 1 * ub)) g1
     in (num % denom, g2)

scatterLines :: (MonadAssembly r c m, Ord r, Fractional r, Random r) => Int -> Brush r c -> (r, r) -> (r, r) -> c -> m ()
scatterLines n brush lb ub bgColor =
  crop lb ub . withBrush brush $ do
    geo $ replicateM_ n (randLSInBox lb ub)
    solidColor bgColor

cefaultScatterLines :: DumbLayer Int PixelRGB8
cefaultScatterLines =
  let brush = def & brushThickness .~ 2 & brushColor ?~ black
      (w, h) = (200, 200)
      gen = mkStdGen 42
      num = 20
      fill = white
      scattered = scatterLines num brush (0, 0) (w, h) fill
      layer :: DumbLayer Rational PixelRGB8
      layer = snd . runIdentity . flip runMonadAssemblyT gen $ scattered
   in sample layer
  where
    sample :: DumbLayer Rational c -> DumbLayer Int c
    sample (DumbLayer l) = DumbLayer (SampleLayer l)

saveScatteredLines :: String -> IO WritingResult
saveScatteredLines = writeDumb8BitBMP cefaultScatterLines