{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module AtTheDisco.Builder where

import AtTheDisco.Geometry
  ( FiniteGeometries (..),
    FiniteGeometry (..),
  )
import AtTheDisco.Layer
  ( DumbLayer (..),
    Layer (ConstantLayer, CropLayer, GeometryLayer, NullLayer),
  )
import Control.Lens (makeLenses, (.~), (?~), (^.))
import Control.Monad (join)
import Control.Monad.RWS (MonadState (get, put, state), RWST, evalRWST)
import Control.Monad.Random (MonadRandom (getRandomR), RandT, evalRandT)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Control.Monad.Writer (MonadWriter (listen, pass, tell, writer), WriterT (runWriterT), censor)
import Data.Default (Default (def))
import Data.Ext (core, type (:+) ((:+)))
import Data.Foldable (Foldable (toList))
import Data.Geometry
  ( LineSegment (ClosedLineSegment),
    Point (Point2),
  )
import Data.Geometry.Box (IsBoxable (boundingBox), Rectangle, boundingBoxList, box, maxPoint, minPoint)
import qualified Data.Geometry.PolyLine as PL
import qualified Data.Geometry.Polygon as PG
import qualified Data.LSeq as LSeq
import System.Random (Random (randomR), Uniform (..), UniformRange, random)
import System.Random.Stateful (StatefulGen)

newtype MonadAssemblyT g r c m a = MonadAssemblyT
  { _runMonadAssemblyT ::
      RWST (Brush r c) (DumbLayer r c) [FiniteGeometry () r] (RandT g m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadRandom,
      MonadWriter (DumbLayer r c),
      MonadReader (Brush r c),
      MonadState [FiniteGeometry () r],
      MonadAssembly r c
    )

runMonadAssemblyT :: (Monad m, Default r) => MonadAssemblyT g r c m a -> g -> m (a, DumbLayer r c)
runMonadAssemblyT m = evalRandT (evalRWST (_runMonadAssemblyT m) def [])

class
  ( Monad m,
    MonadReader (Brush r c) m,
    MonadWriter (DumbLayer r c) m,
    MonadState [FiniteGeometry () r] m,
    MonadRandom m
  ) =>
  MonadAssembly r c m

toPoint2 :: (r, r) -> Point 2 r
toPoint2 = uncurry Point2

blankPoint2 :: (r, r) -> Point 2 r :+ ()
blankPoint2 p = toPoint2 p :+ ()

fg :: [FiniteGeometry p r] -> FiniteGeometries p r
fg = FiniteGeometries . LSeq.promise . LSeq.fromList

-- | A single 'SimplePolygon' in a finite geometry.
--
-- Precondition: at least three points are passed.
pgon' :: (Foldable f, Ord r, Fractional r) => f (r, r) -> [FiniteGeometry () r]
pgon' ps = [ATDSimplePolygon pg]
  where
    -- TODO: should probably do something to make the preconditions work.
    points = map ((:+ ()) . toPoint2) . toList $ ps
    pg = PG.simpleFromPoints points

-- | A single 'LineSegment' in a 'FiniteGeometries'
ls' :: (r, r) -> (r, r) -> [FiniteGeometry () r]
ls' s e = [ATDLineSegment seg]
  where
    seg = ClosedLineSegment (toPoint2 s :+ ()) (toPoint2 e :+ ())

-- | Create a single 'PolyLine' in a 'FiniteGeometries'
--
-- Precondition: at least two points are passed.
pline' :: (Foldable f) => f (r, r) -> [FiniteGeometry () r]
pline' ps = [ATDPolyLine pl]
  where
    points = map ((:+ ()) . toPoint2) . toList $ ps
    pl = PL.fromPointsUnsafe points

addGeometry :: (MonadAssembly r c m) => [FiniteGeometry () r] -> m ()
addGeometry gs = state (\s -> ((), gs <> s))

pgon :: (Foldable f, Ord r, Fractional r, MonadAssembly r c m) => f (r, r) -> m ()
pgon ps = addGeometry (pgon' ps)

pline :: (Foldable f, MonadAssembly r c m) => f (r, r) -> m ()
pline ps = addGeometry (pline' ps)

ls :: (MonadAssembly r c m) => (r, r) -> (r, r) -> m ()
ls p1 p2 = addGeometry (ls' p1 p2)

randPointInBox :: (Ord r, Random r, MonadAssembly r c m) => (r, r) -> (r, r) -> m (r, r)
randPointInBox (x1, y1) (x2, y2) = do
  let (xMin, xMax) = (min x1 x2, max x1 x2)
      (yMin, yMax) = (min y1 y2, max y1 y2)
  x <- getRandomR (xMin, xMax)
  y <- getRandomR (yMin, yMax)
  return (x, y)

randLSInBox :: (Ord r, Random r, MonadAssembly r c m) => (r, r) -> (r, r) -> m ()
randLSInBox lb ub = let rp = randPointInBox lb ub in join $ ls <$> rp <*> rp

-- Brushes

data Brush r c = Brush
  { _brushThickness :: r,
    _brushColor :: Maybe c,
    _brushFill :: DumbLayer r c
  }
  deriving (Show)

$(makeLenses 'Brush)

instance (Default r) => Default (Brush r c) where
  def = Brush def Nothing (DumbLayer NullLayer)

-- Layer Assembly

-- Some shortcuts

withBrush :: (MonadAssembly r c m) => Brush r c -> m a -> m a
withBrush = local . const

withThickness :: (MonadAssembly r c m) => r -> m a -> m a
withThickness t = local (brushThickness .~ t)

withLineColor :: (MonadAssembly r c m) => c -> m a -> m a
withLineColor c = local (brushColor ?~ c)

withoutLineColor :: (MonadAssembly r c m) => m a -> m a
withoutLineColor = local (brushColor .~ Nothing)

withFill :: (MonadAssembly r c m) => DumbLayer r c -> m a -> m a
withFill l = local (brushFill .~ l)

withoutFill :: (MonadAssembly r c m) => m a -> m a
withoutFill = local (brushFill .~ DumbLayer NullLayer)

geo' :: (MonadAssembly r c m, Fractional r, Ord r) => [FiniteGeometry () r] -> m ()
geo' fgs = case fgs of
  [] -> return ()
  _ -> do
    Brush t c (DumbLayer f) <- ask
    tell . DumbLayer $ GeometryLayer (fg fgs) t c f

-- |
geo :: (MonadAssembly r c m, Fractional r, Ord r) => m () -> m ()
geo a = do
  put []
  a
  fgs <- get
  geo' fgs
  put []

solidColor :: (MonadAssembly r c m) => c -> m ()
solidColor = tell . DumbLayer . ConstantLayer

crop :: (MonadAssembly r c m, Ord r) => (r, r) -> (r, r) -> m a -> m a
crop p1 p2 = censor (\(DumbLayer l) -> DumbLayer (CropLayer (box (blankPoint2 p1) (blankPoint2 p2)) l))