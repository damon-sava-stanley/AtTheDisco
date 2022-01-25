{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module AtTheDisco.Layer(
    -- * Types
    Layer, 
    ScreenLayer, 
    ColorLayer,
    -- * util
    unwrapVector,
    mask
    ) where

import Graphics.Color.Model (Color)
import Data.Geometry (Point(..), Arity)

-- | A layer is a named thing.
type Layer v a b = v a -> b

type ScreenLayer v a = Layer v a Bool

type ColorLayer v a m e = Layer v a (Color m e)

unwrapVector :: Layer (Point 2) a b -> a -> a -> b
unwrapVector f x y = f (Point2 x y)

mask :: ScreenLayer v a -> Layer v a b -> Layer v a b -> Layer v a b
mask test onTrue onFalse coord = if test coord then onTrue coord else onFalse coord