{-# LANGUAGE MonoLocalBinds #-}

module AtTheDisco.Sample where

import           AtTheDisco.Layer (Layer)
import           Data.Geometry    (Arity, Point)

-- | A simple "sampling" of a continuous image by an integral one.
subsample ::
     (Num a, Integral b, Arity d) => Layer (Point d) a c -> Layer (Point d) b c
subsample f = f . fmap fromIntegral
