{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

#if __GLASGOW_HASKELL__ >= 705
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase #-}
#endif

#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module Generics.Deriving.Functor (
  -- * Generic Functor class
    GFunctor(..)

  -- * Default method
  , gmapdefault

  -- * Internal Functor class
  , GFunctor'(..)

  ) where

import           Control.Applicative (Const, ZipList)

import qualified Data.Monoid as Monoid (First, Last, Product, Sum)
import           Data.Monoid (Dual)

import           Generics.Deriving.Base

#if MIN_VERSION_base(4,4,0)
import           Data.Complex (Complex)
#endif

#if MIN_VERSION_base(4,6,0)
import           Data.Ord (Down)
#else
import           GHC.Exts (Down)
#endif

#if MIN_VERSION_base(4,7,0)
import           Data.Proxy (Proxy)
#endif

#if MIN_VERSION_base(4,8,0)
import           Data.Functor.Identity (Identity)
import           Data.Monoid (Alt)
#endif

#if MIN_VERSION_base(4,9,0)
import qualified Data.Functor.Product as Functor (Product)
import qualified Data.Functor.Sum as Functor (Sum)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as Semigroup (First, Last)
import           Data.Semigroup (Arg, Max, Min, Option, WrappedMonoid)
#endif

--------------------------------------------------------------------------------
-- Generic fmap
--------------------------------------------------------------------------------

class GFunctor' f where
  gmap' :: (a -> b) -> f a -> f b

instance GFunctor' V1 where
  gmap' _ x = case x of
#if __GLASGOW_HASKELL__ >= 708
                {}
#else
                !_ -> error "Void gmap"
#endif

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
#if __GLASGOW_HASKELL__ >= 701
  default gmap :: (Generic1 f, GFunctor' (Rep1 f))
               => (a -> b) -> f a -> f b
  gmap = gmapdefault
#endif

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

#if MIN_VERSION_base(4,8,0)
instance GFunctor f => GFunctor (Alt f) where
  gmap = gmapdefault
#endif

#if MIN_VERSION_base(4,9,0)
instance GFunctor (Arg a) where
  gmap = gmapdefault
#endif

#if MIN_VERSION_base(4,4,0)
instance GFunctor Complex where
  gmap = gmapdefault
#endif

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

#if MIN_VERSION_base(4,9,0)
instance GFunctor (Semigroup.First) where
  gmap = gmapdefault
#endif

#if MIN_VERSION_base(4,8,0)
instance GFunctor Identity where
  gmap = gmapdefault
#endif

instance GFunctor IO where
  gmap = fmap

instance GFunctor Monoid.Last where
  gmap = gmapdefault

#if MIN_VERSION_base(4,9,0)
instance GFunctor Semigroup.Last where
  gmap = gmapdefault

instance GFunctor Max where
  gmap = gmapdefault
#endif

instance GFunctor Maybe where
  gmap = gmapdefault

#if MIN_VERSION_base(4,9,0)
instance GFunctor Min where
  gmap = gmapdefault

instance GFunctor NonEmpty where
  gmap = gmapdefault

instance GFunctor Option where
  gmap = gmapdefault
#endif

instance GFunctor Monoid.Product where
  gmap = gmapdefault

#if MIN_VERSION_base(4,9,0)
instance (GFunctor f, GFunctor g) => GFunctor (Functor.Product f g) where
  gmap = gmapdefault
#endif

#if MIN_VERSION_base(4,7,0)
instance GFunctor Proxy where
  gmap = gmapdefault
#endif

instance GFunctor Monoid.Sum where
  gmap = gmapdefault

#if MIN_VERSION_base(4,9,0)
instance (GFunctor f, GFunctor g) => GFunctor (Functor.Sum f g) where
  gmap = gmapdefault

instance GFunctor WrappedMonoid where
  gmap = gmapdefault
#endif

instance GFunctor ZipList where
  gmap = gmapdefault
