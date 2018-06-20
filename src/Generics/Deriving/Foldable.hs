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

#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module Generics.Deriving.Foldable (
  -- * Generic Foldable class
    GFoldable(..)

  -- * Default method
  , gfoldMapdefault

  -- * Derived functions
  , gtoList
  , gconcat
  , gconcatMap
  , gand
  , gor
  , gany
  , gall
  , gsum
  , gproduct
  , gmaximum
  , gmaximumBy
  , gminimum
  , gminimumBy
  , gelem
  , gnotElem
  , gfind

  -- * Internal Foldable class
  , GFoldable'(..)
  ) where

import           Control.Applicative (Const, ZipList)

import           Data.Maybe
import qualified Data.Monoid as Monoid (First, Last, Product(..), Sum(..))
import           Data.Monoid (All(..), Any(..), Dual(..), Endo(..))
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (Monoid(..))
#endif

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
#endif

#if MIN_VERSION_base(4,9,0)
import qualified Data.Functor.Product as Functor (Product)
import qualified Data.Functor.Sum as Functor (Sum)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as Semigroup (First, Last)
import           Data.Semigroup (Arg, Max, Min, Option, WrappedMonoid)
#endif

--------------------------------------------------------------------------------
-- Generic fold
--------------------------------------------------------------------------------

class GFoldable' t where
  gfoldMap' :: Monoid m => (a -> m) -> t a -> m

instance GFoldable' V1 where
  gfoldMap' _ _ = mempty

instance GFoldable' U1 where
  gfoldMap' _ U1 = mempty

instance GFoldable' Par1 where
  gfoldMap' f (Par1 a) = f a

instance GFoldable' (K1 i c) where
  gfoldMap' _ (K1 _) = mempty

instance (GFoldable f) => GFoldable' (Rec1 f) where
  gfoldMap' f (Rec1 a) = gfoldMap f a

instance (GFoldable' f) => GFoldable' (M1 i c f) where
  gfoldMap' f (M1 a) = gfoldMap' f a

instance (GFoldable' f, GFoldable' g) => GFoldable' (f :+: g) where
  gfoldMap' f (L1 a) = gfoldMap' f a
  gfoldMap' f (R1 a) = gfoldMap' f a

instance (GFoldable' f, GFoldable' g) => GFoldable' (f :*: g) where
  gfoldMap' f (a :*: b) = mappend (gfoldMap' f a) (gfoldMap' f b)

instance (GFoldable f, GFoldable' g) => GFoldable' (f :.: g) where
  gfoldMap' f (Comp1 x) = gfoldMap (gfoldMap' f) x

instance GFoldable' UAddr where
  gfoldMap' _ (UAddr _) = mempty

instance GFoldable' UChar where
  gfoldMap' _ (UChar _) = mempty

instance GFoldable' UDouble where
  gfoldMap' _ (UDouble _) = mempty

instance GFoldable' UFloat where
  gfoldMap' _ (UFloat _) = mempty

instance GFoldable' UInt where
  gfoldMap' _ (UInt _) = mempty

instance GFoldable' UWord where
  gfoldMap' _ (UWord _) = mempty

class GFoldable t where
  gfoldMap :: Monoid m => (a -> m) -> t a -> m
#if __GLASGOW_HASKELL__ >= 701
  default gfoldMap :: (Generic1 t, GFoldable' (Rep1 t), Monoid m)
                   => (a -> m) -> t a -> m
  gfoldMap = gfoldMapdefault
#endif

  gfold :: Monoid m => t m -> m
  gfold = gfoldMap id

  gfoldr :: (a -> b -> b) -> b -> t a -> b
  gfoldr f z t = appEndo (gfoldMap (Endo . f) t) z

  gfoldr' :: (a -> b -> b) -> b -> t a -> b
  gfoldr' f z0 xs = gfoldl f' id xs z0
    where f' k x z = k $! f x z

  gfoldl :: (a -> b -> a) -> a -> t b -> a
  gfoldl f z t = appEndo (getDual (gfoldMap (Dual . Endo . flip f) t)) z

  gfoldl' :: (a -> b -> a) -> a -> t b -> a
  gfoldl' f z0 xs = gfoldr f' id xs z0
    where f' x k z = k $! f z x

  gfoldr1 :: (a -> a -> a) -> t a -> a
  gfoldr1 f xs = fromMaybe (error "gfoldr1: empty structure")
                   (gfoldr mf Nothing xs)
    where
      mf x Nothing = Just x
      mf x (Just y) = Just (f x y)

  gfoldl1 :: (a -> a -> a) -> t a -> a
  gfoldl1 f xs = fromMaybe (error "foldl1: empty structure")
                   (gfoldl mf Nothing xs)
    where
      mf Nothing y = Just y
      mf (Just x) y = Just (f x y)

gfoldMapdefault :: (Generic1 t, GFoldable' (Rep1 t), Monoid m)
                => (a -> m) -> t a -> m
gfoldMapdefault f x = gfoldMap' f (from1 x)

-- Base types instances
instance GFoldable ((,) a) where
  gfoldMap = gfoldMapdefault

instance GFoldable [] where
  gfoldMap = gfoldMapdefault

#if MIN_VERSION_base(4,9,0)
instance GFoldable (Arg a) where
  gfoldMap = gfoldMapdefault
#endif

#if MIN_VERSION_base(4,4,0)
instance GFoldable Complex where
  gfoldMap = gfoldMapdefault
#endif

instance GFoldable (Const m) where
  gfoldMap = gfoldMapdefault

instance GFoldable Down where
  gfoldMap = gfoldMapdefault

instance GFoldable Dual where
  gfoldMap = gfoldMapdefault

instance GFoldable (Either a) where
  gfoldMap = gfoldMapdefault

instance GFoldable Monoid.First where
  gfoldMap = gfoldMapdefault

#if MIN_VERSION_base(4,9,0)
instance GFoldable (Semigroup.First) where
  gfoldMap = gfoldMapdefault
#endif

#if MIN_VERSION_base(4,8,0)
instance GFoldable Identity where
  gfoldMap = gfoldMapdefault
#endif

instance GFoldable Monoid.Last where
  gfoldMap = gfoldMapdefault

#if MIN_VERSION_base(4,9,0)
instance GFoldable Semigroup.Last where
  gfoldMap = gfoldMapdefault

instance GFoldable Max where
  gfoldMap = gfoldMapdefault
#endif

instance GFoldable Maybe where
  gfoldMap = gfoldMapdefault

#if MIN_VERSION_base(4,9,0)
instance GFoldable Min where
  gfoldMap = gfoldMapdefault

instance GFoldable NonEmpty where
  gfoldMap = gfoldMapdefault

instance GFoldable Option where
  gfoldMap = gfoldMapdefault
#endif

instance GFoldable Monoid.Product where
  gfoldMap = gfoldMapdefault

#if MIN_VERSION_base(4,9,0)
instance (GFoldable f, GFoldable g) => GFoldable (Functor.Product f g) where
  gfoldMap = gfoldMapdefault
#endif

#if MIN_VERSION_base(4,7,0)
instance GFoldable Proxy where
  gfoldMap = gfoldMapdefault
#endif

instance GFoldable Monoid.Sum where
  gfoldMap = gfoldMapdefault

#if MIN_VERSION_base(4,9,0)
instance (GFoldable f, GFoldable g) => GFoldable (Functor.Sum f g) where
  gfoldMap = gfoldMapdefault

instance GFoldable WrappedMonoid where
  gfoldMap = gfoldMapdefault
#endif

instance GFoldable ZipList where
  gfoldMap = gfoldMapdefault

gtoList :: GFoldable t => t a -> [a]
gtoList = gfoldr (:) []

gconcat :: GFoldable t => t [a] -> [a]
gconcat = gfold

gconcatMap :: GFoldable t => (a -> [b]) -> t a -> [b]
gconcatMap = gfoldMap

gand :: GFoldable t => t Bool -> Bool
gand = getAll . gfoldMap All

gor :: GFoldable t => t Bool -> Bool
gor = getAny . gfoldMap Any

gany :: GFoldable t => (a -> Bool) -> t a -> Bool
gany p = getAny . gfoldMap (Any . p)

gall :: GFoldable t => (a -> Bool) -> t a -> Bool
gall p = getAll . gfoldMap (All . p)

gsum :: (GFoldable t, Num a) => t a -> a
gsum = Monoid.getSum . gfoldMap Monoid.Sum

gproduct :: (GFoldable t, Num a) => t a -> a
gproduct = Monoid.getProduct . gfoldMap Monoid.Product

gmaximum :: (GFoldable t, Ord a) => t a -> a
gmaximum = gfoldr1 max

gmaximumBy :: GFoldable t => (a -> a -> Ordering) -> t a -> a
gmaximumBy cmp = gfoldr1 max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y

gminimum :: (GFoldable t, Ord a) => t a -> a
gminimum = gfoldr1 min

gminimumBy :: GFoldable t => (a -> a -> Ordering) -> t a -> a
gminimumBy cmp = gfoldr1 min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x

gelem :: (GFoldable t, Eq a) => a -> t a -> Bool
gelem = gany . (==)

gnotElem :: (GFoldable t, Eq a) => a -> t a -> Bool
gnotElem x = not . gelem x

gfind :: GFoldable t => (a -> Bool) -> t a -> Maybe a
gfind p = listToMaybe . gconcatMap (\ x -> if p x then [x] else [])
