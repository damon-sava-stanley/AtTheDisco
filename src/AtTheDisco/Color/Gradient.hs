{-# LANGUAGE DeriveFunctor #-}

module AtTheDisco.Color.Gradient
  ( Gradient
  , newGradient
  , gradientFromList
  , insertKeypoint
  , gradientValue
  , colorGradientValue
  ) where

import           AtTheDisco.Color.Combination (blendColors)
import           Data.List                    (sortBy)

-- | A gradient is a colored line segment that smoothly blends between values
-- specified at the point. It must at least have a start and end point.
newtype Gradient e a =
  Gradient [(a, e)]
  deriving (Eq, Ord, Show, Functor)

-- | Create a new gradient with the specified start and end point at 0 and 1.
newGradient :: Num e => a -> a -> Gradient e a
newGradient start end = Gradient [(start, 0), (end, 1)]

-- | Create a new gradient from a nonempty list. Returns Nothing on an empty list.
gradientFromList :: Ord e => [(a, e)] -> Maybe (Gradient e a)
gradientFromList [] = Nothing
gradientFromList xs =
  Just . Gradient $ sortBy (\(_, p) (_, p') -> compare p p') xs

-- | Add a new keypoint to the gradient. The point should be in the range (0, 1)
insertKeypoint :: Ord e => a -> e -> Gradient e a -> Gradient e a
insertKeypoint val point (Gradient vals) = Gradient $ insertInner val point vals
  where
    insertInner v p g@((vI, pI):gs)
      | p < pI = (v, p) : g
      | p == pI = (v, p) : gs
      | otherwise = (vI, pI) : insertInner v p gs
    insertInner v p [] = [(v, p)]

-- | Retrieve the value of the gradient at a spot. If the requested value is
-- outside of the defined range, either the starting or end value will be
-- returned. If it is within the range, the defined points surrounding the
gradientValue ::
     (Ord t, Fractional t)
  => (t -> p -> p -> p) -- ^ The blend function. First value will be between 0 and 1
  -> t -- ^ The requested location
  -> Gradient t p -- ^ The gradient
  -> p
gradientValue blend x (Gradient gs) = go x gs
  where
    go x g@((v, p):(v', p'):gs)
      | x < p || x == p = v
      | x > p && x < p' = blend ((x - p) / (p' - p)) v v'
      | otherwise = go x (tail g)
    go x [(v, p)] = v
    go x [] = error "Empty gradient" -- Cannot reach this case as Gradient cannot be constructed empty

-- | A specialization of 'gradientValue' which uses 'blendColors'
colorGradientValue ::
     (Applicative f, RealFloat e)
  => e -- ^ The requested location
  -> Gradient e (f e) -- ^ The gradient
  -> f e
colorGradientValue = gradientValue (\d x y -> blendColors d y x)
