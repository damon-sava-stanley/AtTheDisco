{-# LANGUAGE DataKinds #-}

-- | Functions for constructing gradients
module AtTheDisco.Geometry.Gradient where

import           Data.Geometry (Affine ((.-.)), Point, Vector, dot, quadrance)

lineSegmentGradient :: (Floating e) => Point 2 e -> Vector 2 e -> Point 2 e -> e
lineSegmentGradient o v p = let u = p .-. o in (u `dot` v) / quadrance v
