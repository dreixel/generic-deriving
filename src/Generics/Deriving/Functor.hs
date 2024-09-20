{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Generics.Deriving.Functor (
  -- * Generic Functor class
    GFunctor(..)

  -- * Default method
  , gmapdefault

  -- * Internal Functor class
  , GFunctor'(..)

  ) where

import           Control.Applicative (Const, ZipList)

import           Data.Complex (Complex)
import           Data.Functor.Identity (Identity)
import qualified Data.Functor.Product as Functor (Product)
import qualified Data.Functor.Sum as Functor (Sum)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Monoid as Monoid (First, Last, Product, Sum)
import           Data.Monoid (Alt, Dual)
import           Data.Ord (Down)
import           Data.Proxy (Proxy)
import qualified Data.Semigroup as Semigroup (First, Last)
import           Data.Semigroup (Arg, Max, Min, WrappedMonoid)

import           Generics.Deriving.Base

--------------------------------------------------------------------------------
-- Generic fmap
--------------------------------------------------------------------------------

class GFunctor' f where
  gmap' :: (a -> b) -> f a -> f b

instance GFunctor' V1 where
  gmap' _ x = case x of {}

instance GFunctor' U1 where
  gmap' _ U1 = U1

instance GFunctor' Par1 where
  gmap' f (Par1 a) = Par1 (f a)

instance GFunctor' (K1 i c) where
  gmap' _ (K1 a) = K1 a

instance (GFunctor f) => GFunctor' (Rec1 f) where
  gmap' f (Rec1 a) = Rec1 (gmap f a)

instance (GFunctor' f) => GFunctor' (M1 i c f) where
  gmap' f (M1 a) = M1 (gmap' f a)

instance (GFunctor' f, GFunctor' g) => GFunctor' (f :+: g) where
  gmap' f (L1 a) = L1 (gmap' f a)
  gmap' f (R1 a) = R1 (gmap' f a)

instance (GFunctor' f, GFunctor' g) => GFunctor' (f :*: g) where
  gmap' f (a :*: b) = gmap' f a :*: gmap' f b

instance (GFunctor f, GFunctor' g) => GFunctor' (f :.: g) where
  gmap' f (Comp1 x) = Comp1 (gmap (gmap' f) x)

instance GFunctor' UAddr where
  gmap' _ (UAddr a) = UAddr a

instance GFunctor' UChar where
  gmap' _ (UChar c) = UChar c

instance GFunctor' UDouble where
  gmap' _ (UDouble d) = UDouble d

instance GFunctor' UFloat where
  gmap' _ (UFloat f) = UFloat f

instance GFunctor' UInt where
  gmap' _ (UInt i) = UInt i

instance GFunctor' UWord where
  gmap' _ (UWord w) = UWord w

class GFunctor f where
  gmap :: (a -> b) -> f a -> f b
  default gmap :: (Generic1 f, GFunctor' (Rep1 f))
               => (a -> b) -> f a -> f b
  gmap = gmapdefault

gmapdefault :: (Generic1 f, GFunctor' (Rep1 f))
            => (a -> b) -> f a -> f b
gmapdefault f = to1 . gmap' f . from1

-- Base types instances
instance GFunctor ((->) r) where
  gmap = fmap

instance GFunctor ((,) a) where
  gmap = gmapdefault

instance GFunctor [] where
  gmap = gmapdefault

instance GFunctor f => GFunctor (Alt f) where
  gmap = gmapdefault

instance GFunctor (Arg a) where
  gmap = gmapdefault

instance GFunctor Complex where
  gmap = gmapdefault

instance GFunctor (Const m) where
  gmap = gmapdefault

instance GFunctor Down where
  gmap = gmapdefault

instance GFunctor Dual where
  gmap = gmapdefault

instance GFunctor (Either a) where
  gmap = gmapdefault

instance GFunctor Monoid.First where
  gmap = gmapdefault

instance GFunctor (Semigroup.First) where
  gmap = gmapdefault

instance GFunctor Identity where
  gmap = gmapdefault

instance GFunctor IO where
  gmap = fmap

instance GFunctor Monoid.Last where
  gmap = gmapdefault

instance GFunctor Semigroup.Last where
  gmap = gmapdefault

instance GFunctor Max where
  gmap = gmapdefault

instance GFunctor Maybe where
  gmap = gmapdefault

instance GFunctor Min where
  gmap = gmapdefault

instance GFunctor NonEmpty where
  gmap = gmapdefault

instance GFunctor Monoid.Product where
  gmap = gmapdefault

instance (GFunctor f, GFunctor g) => GFunctor (Functor.Product f g) where
  gmap = gmapdefault

instance GFunctor Proxy where
  gmap = gmapdefault

instance GFunctor Monoid.Sum where
  gmap = gmapdefault

instance (GFunctor f, GFunctor g) => GFunctor (Functor.Sum f g) where
  gmap = gmapdefault

instance GFunctor WrappedMonoid where
  gmap = gmapdefault

instance GFunctor ZipList where
  gmap = gmapdefault
