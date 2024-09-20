{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Generics.Deriving.Traversable (
  -- * Generic Traversable class
    GTraversable(..)

  -- * Default method
  , gtraversedefault

  -- * Internal Traversable class
  , GTraversable'(..)

  ) where

import           Control.Applicative (Const, WrappedMonad(..), ZipList)

import           Data.Complex (Complex)
import           Data.Functor.Identity (Identity)
import qualified Data.Functor.Product as Functor (Product)
import qualified Data.Functor.Sum as Functor (Sum)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Monoid as Monoid (First, Last, Product, Sum)
import           Data.Monoid (Dual)
import           Data.Ord (Down)
import           Data.Proxy (Proxy)
import qualified Data.Semigroup as Semigroup (First, Last)
import           Data.Semigroup (Arg, Max, Min, WrappedMonoid)

import           Generics.Deriving.Base
import           Generics.Deriving.Foldable
import           Generics.Deriving.Functor

--------------------------------------------------------------------------------
-- Generic traverse
--------------------------------------------------------------------------------

class GTraversable' t where
  gtraverse' :: Applicative f => (a -> f b) -> t a -> f (t b)

instance GTraversable' V1 where
  gtraverse' _ x = pure $ case x of {}

instance GTraversable' U1 where
  gtraverse' _ U1 = pure U1

instance GTraversable' Par1 where
  gtraverse' f (Par1 a) = Par1 <$> f a

instance GTraversable' (K1 i c) where
  gtraverse' _ (K1 a) = pure (K1 a)

instance (GTraversable f) => GTraversable' (Rec1 f) where
  gtraverse' f (Rec1 a) = Rec1 <$> gtraverse f a

instance (GTraversable' f) => GTraversable' (M1 i c f) where
  gtraverse' f (M1 a) = M1 <$> gtraverse' f a

instance (GTraversable' f, GTraversable' g) => GTraversable' (f :+: g) where
  gtraverse' f (L1 a) = L1 <$> gtraverse' f a
  gtraverse' f (R1 a) = R1 <$> gtraverse' f a

instance (GTraversable' f, GTraversable' g) => GTraversable' (f :*: g) where
  gtraverse' f (a :*: b) = (:*:) <$> gtraverse' f a <*> gtraverse' f b

instance (GTraversable f, GTraversable' g) => GTraversable' (f :.: g) where
  gtraverse' f (Comp1 x) = Comp1 <$> gtraverse (gtraverse' f) x

instance GTraversable' UAddr where
  gtraverse' _ (UAddr a) = pure (UAddr a)

instance GTraversable' UChar where
  gtraverse' _ (UChar c) = pure (UChar c)

instance GTraversable' UDouble where
  gtraverse' _ (UDouble d) = pure (UDouble d)

instance GTraversable' UFloat where
  gtraverse' _ (UFloat f) = pure (UFloat f)

instance GTraversable' UInt where
  gtraverse' _ (UInt i) = pure (UInt i)

instance GTraversable' UWord where
  gtraverse' _ (UWord w) = pure (UWord w)

class (GFunctor t, GFoldable t) => GTraversable t where
  gtraverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  default gtraverse :: (Generic1 t, GTraversable' (Rep1 t), Applicative f)
                    => (a -> f b) -> t a -> f (t b)
  gtraverse = gtraversedefault

  gsequenceA :: Applicative f => t (f a) -> f (t a)
  gsequenceA = gtraverse id

  gmapM :: Monad m => (a -> m b) -> t a -> m (t b)
  gmapM f = unwrapMonad . gtraverse (WrapMonad . f)

  gsequence :: Monad m => t (m a) -> m (t a)
  gsequence = gmapM id

gtraversedefault :: (Generic1 t, GTraversable' (Rep1 t), Applicative f)
                 => (a -> f b) -> t a -> f (t b)
gtraversedefault f x = to1 <$> gtraverse' f (from1 x)

-- Base types instances
instance GTraversable ((,) a) where
  gtraverse = gtraversedefault

instance GTraversable [] where
  gtraverse = gtraversedefault

instance GTraversable (Arg a) where
  gtraverse = gtraversedefault

instance GTraversable Complex where
  gtraverse = gtraversedefault

instance GTraversable (Const m) where
  gtraverse = gtraversedefault

instance GTraversable Down where
  gtraverse = gtraversedefault

instance GTraversable Dual where
  gtraverse = gtraversedefault

instance GTraversable (Either a) where
  gtraverse = gtraversedefault

instance GTraversable Monoid.First where
  gtraverse = gtraversedefault

instance GTraversable (Semigroup.First) where
  gtraverse = gtraversedefault

instance GTraversable Identity where
  gtraverse = gtraversedefault

instance GTraversable Monoid.Last where
  gtraverse = gtraversedefault

instance GTraversable Semigroup.Last where
  gtraverse = gtraversedefault

instance GTraversable Max where
  gtraverse = gtraversedefault

instance GTraversable Maybe where
  gtraverse = gtraversedefault

instance GTraversable Min where
  gtraverse = gtraversedefault

instance GTraversable NonEmpty where
  gtraverse = gtraversedefault

instance GTraversable Monoid.Product where
  gtraverse = gtraversedefault

instance (GTraversable f, GTraversable g) => GTraversable (Functor.Product f g) where
  gtraverse = gtraversedefault

instance GTraversable Proxy where
  gtraverse = gtraversedefault

instance GTraversable Monoid.Sum where
  gtraverse = gtraversedefault

instance (GTraversable f, GTraversable g) => GTraversable (Functor.Sum f g) where
  gtraverse = gtraversedefault

instance GTraversable WrappedMonoid where
  gtraverse = gtraversedefault

instance GTraversable ZipList where
  gtraverse = gtraversedefault
