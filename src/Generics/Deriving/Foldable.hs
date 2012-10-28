{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

module Generics.Deriving.Foldable (
  -- * Foldable class
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
  ) where

import Data.Maybe
import Data.Monoid
import Generics.Deriving.Base
import Generics.Deriving.Instances ()

--------------------------------------------------------------------------------
-- Generic fold
--------------------------------------------------------------------------------

class GFoldable' t where
  gfoldMap' :: Monoid m => (a -> m) -> t a -> m

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
instance GFoldable Maybe where
  gfoldMap = gfoldMapdefault

instance GFoldable [] where
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
gsum = getSum . gfoldMap Sum

gproduct :: (GFoldable t, Num a) => t a -> a
gproduct = getProduct . gfoldMap Product

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

