{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AtTheDisco.Color.Combination where

import           AtTheDisco.Layer     (Layer)
import           Control.Applicative  (Applicative (liftA2))
import           Graphics.Color.Model (Alpha, Color,
                                       Elevator (fromDouble, toDouble),
                                       addAlpha, dropAlpha, getAlpha,
                                       modifyOpaque)

blendColors :: (Applicative f, RealFloat e) => e -> f e -> f e -> f e
blendColors x c1 c2 = (+) <$> fmap (x *) c1 <*> fmap ((1 - x) *) c2

blendLayers ::
     (Applicative f1, Applicative f2, RealFloat e)
  => e
  -> f1 (f2 e)
  -> f1 (f2 e)
  -> f1 (f2 e)
blendLayers x l1 l2 = blendColors x <$> l1 <*> l2

multiplyColors :: (Applicative f, RealFloat b) => f b -> f b -> f b
multiplyColors c1 c2 = (*) <$> c1 <*> c2

multiplyLayers ::
     (Applicative f1, Applicative f2, RealFloat b)
  => f1 (f2 b)
  -> f1 (f2 b)
  -> f1 (f2 b)
multiplyLayers l1 l2 = multiplyColors <$> l1 <*> l2

screenColors :: (Applicative f, RealFloat b) => f b -> f b -> f b
screenColors c1 c2 = screen <$> c1 <*> c2
  where
    screen a b = 1 - (1 - a) * (1 - b)

screenLayers ::
     (Applicative f1, Applicative f2, RealFloat b)
  => f1 (f2 b)
  -> f1 (f2 b)
  -> f1 (f2 b)
screenLayers l1 l2 = screenColors <$> l1 <*> l2

newtype Premultiplied a =
  Premultiplied a
  deriving (Eq, Ord, Show, Read, Functor)

instance Applicative Premultiplied where
  pure = Premultiplied
  liftA2 f (Premultiplied a) (Premultiplied b) = Premultiplied (f a b)

-- | Premultiplying the alpha channel of a layer can speed up composition of that layer.
premultiply ::
     (Elevator e, Functor (Color cs))
  => Color (Alpha cs) e
  -> Premultiplied (Color (Alpha cs) e)
premultiply c =
  let a = toDouble . getAlpha $ c
   in Premultiplied $ modifyOpaque (fmap (fromDouble . (* a) . toDouble)) c

unPremultiply ::
     (Functor (Color cs), Elevator e)
  => Premultiplied (Color (Alpha cs) e)
  -> Color (Alpha cs) e
unPremultiply (Premultiplied c) =
  let a = toDouble . getAlpha $ c
   in modifyOpaque (fmap (fromDouble . (/ a) . toDouble)) c

class AlphaOver a where
  atop :: a -> a -> a

instance (Elevator e, Applicative (Color cs)) =>
         AlphaOver (Color (Alpha cs) e) where
  atop c1 c2 =
    let c1' = fmap toDouble c1
        c2' = fmap toDouble c2
        a1 = getAlpha c1'
        a2 = getAlpha c2'
        a3 = a1 + a2 * (1 - a1)
        combineColor ch1 ch2 = (ch1 * a1 + ch2 * a2 * (1 - a1))
        combined = combineColor <$> dropAlpha c1' <*> dropAlpha c2'
     in fromDouble <$> addAlpha combined a3

instance (Elevator e, Applicative (Color cs)) =>
         AlphaOver (Premultiplied (Color (Alpha cs) e)) where
  atop (Premultiplied c1) (Premultiplied c2) =
    let c1' = fmap toDouble c1
        c2' = fmap toDouble c2
        a1 = getAlpha c1'
     in Premultiplied $ fromDouble <$> ((+) <$> c1' <*> fmap (* (1 - a1)) c2')

instance AlphaOver a => AlphaOver (Layer p c a) where
  atop l1 l2 = atop <$> l1 <*> l2
