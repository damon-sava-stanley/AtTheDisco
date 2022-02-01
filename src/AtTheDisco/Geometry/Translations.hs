{-# LANGUAGE DataKinds #-}

module AtTheDisco.Geometry.Translations where

import           AtTheDisco.Layer (Layer)
import           Data.Geometry    (IsTransformable (transformBy),
                                   Point (Point2, toVec), Vector, negated,
                                   reflection, rotation, scaling, translation,
                                   (|.|), xCoord, yCoord, norm, vector)
import Control.Lens ((^.))

translateLayer ::
     (Floating r) => Vector 2 r -> Layer (Point 2) r o -> Layer (Point 2) r o
translateLayer v l = l . transformBy (translation (negated v))

rotateLayerAbout ::
     (Floating r)
  => Point 2 r
  -> r
  -> Layer (Point 2) r o
  -> Layer (Point 2) r o
rotateLayerAbout origin rad l =
  l .
  transformBy
    (translation vo |.| rotation (negate rad) |.| translation (negated vo))
  where
    vo = toVec origin

scaleLayerAbout ::
     (Floating r)
  => Point 2 r
  -> Vector 2 r
  -> Layer (Point 2) r o
  -> Layer (Point 2) r o
scaleLayerAbout o sv l =
  l . transformBy (translation vo |.| scaling isv |.| translation (negated vo))
  where
    isv = fmap (1 /) sv
    vo = toVec o

reflectLayerAbout ::
     (Floating r)
  => Point 2 r
  -> r
  -> Layer (Point 2) r o
  -> Layer (Point 2) r o
reflectLayerAbout o r l =
  l .
  transformBy
    (translation vo |.| reflection (negate r) |.| translation (negated vo))
  where
    vo = toVec o

toPolarLayer :: (RealFloat r) => Layer (Point 2) r o -> Layer (Point 2) r o
toPolarLayer l p =
  let x = p ^. xCoord
      y = p ^. yCoord
      r = norm $ p ^. vector
      theta = atan2 x y
   in l $ Point2 r theta
