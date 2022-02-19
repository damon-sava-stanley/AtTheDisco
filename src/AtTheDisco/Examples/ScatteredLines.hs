module AtTheDisco.Examples.ScatteredLines where

import AtTheDisco.Builder
import AtTheDisco.Layer (DumbLayer)
import Codec.Picture (PixelRGB8 (PixelRGB8))
import Control.Lens ( (&), (.~), (?~) )
import Control.Monad (replicateM_)
import Control.Monad.Random (Random, mkStdGen)
import Data.Default (Default (def))
import Data.Functor.Identity (Identity (runIdentity))

black :: PixelRGB8
black = PixelRGB8 0 0 0

white :: PixelRGB8
white = PixelRGB8 0 0 0

scatterLines :: (MonadAssembly r c m, Ord r, Fractional r, Random r) => Int -> Brush r c -> (r, r) -> (r, r) -> c -> m ()
scatterLines n brush lb ub bgColor =
  crop lb ub . withBrush brush $ do
    geo $ replicateM_ n (randLSInBox lb ub)
    solidColor bgColor

runDefaultScatterLines :: DumbLayer Double PixelRGB8
runDefaultScatterLines =
  snd . runIdentity $
    runMonadAssemblyT
      ( scatterLines
          100
          ( def & brushThickness .~ 5
              & brushColor ?~ black
          )
          (0 :: Double, 0)
          (1000, 1000)
          white
      )
      (mkStdGen 42)