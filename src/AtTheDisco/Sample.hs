module AtTheDisco.Sample where

import AtTheDisco.Layer (Layer)
import Data.Vector.Fixed (Vector)
import qualified Data.Vector.Fixed as VF

-- | A simple "sampling" of a continuous image by an integral one. 
subsample :: (Num a, Integral b, Vector v a, Vector v b) 
          => Layer v a c -> Layer v b c
subsample f = f . VF.map fromIntegral

