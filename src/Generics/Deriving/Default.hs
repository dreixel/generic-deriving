-- |
-- Module      : Generics.Deriving.Default
-- Description : Default implementations of generic classes
-- License     : BSD-3-Clause
--
-- Maintainer  : generics@haskell.org
-- Stability   : experimental
-- Portability : non-portable
--
-- GHC 8.6 introduced the 'DerivingVia' language extension, which means
-- an instance of a typeclass for a type can be derived from an instance
-- for an isomorphic type.
--
-- There are a number of default instantiations for the classes to be
-- derived via generics in this package. This module exports the 'Default'
-- newtype, which indicates that a type should use those default instances
-- for its typeclass instances.
--
-- Use as follows:
--
-- @
-- data MyType = …
--  deriving (Generic)
--  deriving (Eq) via (Default MyType)
--
-- deriving via (Default MyType) instance Show MyType
-- @

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE InstanceSigs #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Deriving.Default
  ( Default(..)
  ) where

import Data.Foldable

import Generics.Deriving.Base
import Generics.Deriving.Copoint
import Generics.Deriving.Enum
import Generics.Deriving.Eq
import Generics.Deriving.Foldable
import Generics.Deriving.Functor
import Generics.Deriving.Monoid
import Generics.Deriving.Semigroup
import Generics.Deriving.Show
import Generics.Deriving.Uniplate

newtype Default a = Default { unDefault :: a }

newtype Default1 f a = Default1 { unDefault1 :: f a }

--------------------------------------------------------------------------------
-- Eq
--------------------------------------------------------------------------------

instance (Generic a, GEq' (Rep a)) => Eq (Default a) where
#if __GLASGOW_HASKELL__ >= 706
  (==) :: Default a -> Default a -> Bool
#endif
  Default x == Default y = x `geqdefault` y

instance (Generic a, GEq' (Rep a)) => GEq (Default a) where
#if __GLASGOW_HASKELL__ >= 706
  geq :: Default a -> Default a -> Bool
#endif
  Default x `geq` Default y = x `geqdefault` y

--------------------------------------------------------------------------------
-- Enum
--------------------------------------------------------------------------------

instance (Generic a, GEq a, Enum' (Rep a)) => Enum (Default a) where
#if __GLASGOW_HASKELL__ >= 706
  toEnum :: Int -> Default a
  fromEnum :: Default a -> Int
#endif
  toEnum = Default . toEnumDefault
  fromEnum (Default x) = fromEnumDefault x

instance (Generic a, GEq a, Enum' (Rep a)) => GEnum (Default a) where
#if __GLASGOW_HASKELL__ >= 706
  genum :: [Default a]
#endif
  genum = Default . to <$> enum'

--------------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------------

instance (Generic a, GShow' (Rep a)) => Show (Default a) where
#if __GLASGOW_HASKELL__ >= 706
  showsPrec :: Int -> Default a -> ShowS
#endif
  showsPrec n (Default x) = gshowsPrecdefault n x

instance (Generic a, GShow' (Rep a)) => GShow (Default a) where
#if __GLASGOW_HASKELL__ >= 706
  gshowsPrec :: Int -> Default a -> ShowS
#endif
  gshowsPrec n (Default x) = gshowsPrecdefault n x

--------------------------------------------------------------------------------
-- Semigroup
--------------------------------------------------------------------------------

instance (Generic a, GSemigroup' (Rep a)) => Semigroup (Default a) where
#if __GLASGOW_HASKELL__ >= 706
  (<>) :: Default a -> Default a -> Default a
#endif
  Default x <> Default y = Default $ x `gsappenddefault` y

instance (Generic a, GSemigroup' (Rep a)) => GSemigroup (Default a) where
#if __GLASGOW_HASKELL__ >= 706
  gsappend :: Default a -> Default a -> Default a
#endif
  Default x `gsappend` Default y = Default $ x `gsappenddefault` y

--------------------------------------------------------------------------------
-- Monoid
--------------------------------------------------------------------------------

#if (MIN_VERSION_base(4,11,0))
instance (Generic a, GMonoid' (Rep a), Semigroup a, GSemigroup' (Rep a)) => Monoid (Default a) where
#else
instance (Generic a, GMonoid' (Rep a)) => Monoid (Default a) where
#endif
#if __GLASGOW_HASKELL__ >= 706
  mempty :: Default a
  mappend :: Default a -> Default a -> Default a
#endif
  mempty = Default gmemptydefault
  Default x `mappend` Default y = Default $ x `gmappenddefault` y

instance (Generic a, GMonoid' (Rep a)) => GMonoid (Default a) where
#if __GLASGOW_HASKELL__ >= 706
  gmempty :: Default a
  gmappend :: Default a -> Default a -> Default a
#endif
  gmempty = Default gmemptydefault
  Default x `gmappend` Default y = Default $ x `gmappenddefault` y

--------------------------------------------------------------------------------
-- Uniplate
--------------------------------------------------------------------------------

instance (Generic a, Uniplate' (Rep a) a, Context' (Rep a) a) => Uniplate (Default a) where

#if __GLASGOW_HASKELL__ >= 706
  children   ::                                             Default a  ->   [Default a]
  context    ::                             Default a   -> [Default a] ->    Default a
  descend    ::            (Default a ->    Default a)  ->  Default a  ->    Default a
  descendM   :: Monad m => (Default a -> m (Default a)) ->  Default a  -> m (Default a)
  transform  ::            (Default a ->    Default a)  ->  Default a  ->    Default a
  transformM :: Monad m => (Default a -> m (Default a)) ->  Default a  -> m (Default a)
#endif

  children     (Default x)    = Default <$> childrendefault    x
  context      (Default x) ys = Default  $  contextdefault     x   (unDefault <$> ys)
  descend    f (Default x)    = Default  $  descenddefault         (unDefault . f . Default) x
  descendM   f (Default x)    = Default <$> descendMdefault   (fmap unDefault . f . Default) x
  transform  f (Default x)    = Default  $  transformdefault       (unDefault . f . Default) x
  transformM f (Default x)    = Default <$> transformMdefault (fmap unDefault . f . Default) x

--------------------------------------------------------------------------------
-- Functor
--------------------------------------------------------------------------------

instance (Generic1 f, GFunctor' (Rep1 f)) => Functor (Default1 f) where
  fmap = gmap

instance (Generic1 f, GFunctor' (Rep1 f)) => GFunctor (Default1 f) where
#if __GLASGOW_HASKELL__ >= 706
  gmap :: (a -> b) -> (Default1 f) a -> (Default1 f) b
#endif
  gmap f (Default1 fx) = Default1 $ gmapdefault f fx

--------------------------------------------------
-- Copoint
--------------------------------------------------

instance (Generic1 f, GCopoint' (Rep1 f)) => GCopoint (Default1 f) where
#if __GLASGOW_HASKELL__ >= 706
  gcopoint :: Default1 f a -> a
#endif
  gcopoint = gcopointdefault . unDefault1

--------------------------------------------------
-- Foldable
--------------------------------------------------

instance (Generic1 t, GFoldable' (Rep1 t)) => Foldable (Default1 t) where
#if __GLASGOW_HASKELL__ >= 706
  foldMap :: (Monoid m) => (a -> m) -> Default1 t a -> m
  fold    ::  Monoid m              => Default1 t m -> m
  foldr   :: (a -> b -> b)    -> b  -> Default1 t a -> b
  foldr'  :: (a -> b -> b)    -> b  -> Default1 t a -> b
  foldl   :: (a -> b -> a)    -> a  -> Default1 t b -> a
  foldl'  :: (a -> b -> a)    -> a  -> Default1 t b -> a
  foldr1  :: (a -> a -> a)          -> Default1 t a -> a
  foldl1  :: (a -> a -> a)          -> Default1 t a -> a
#endif
  foldMap = gfoldMap
  fold    = gfold
  foldr   = gfoldr
  foldr'  = gfoldr'
  foldl   = gfoldl
  foldl'  = gfoldl'
  foldr1  = gfoldr1
  foldl1  = gfoldl1

instance (Generic1 t, GFoldable' (Rep1 t)) => GFoldable (Default1 t) where
#if __GLASGOW_HASKELL__ >= 706
  gfoldMap :: Monoid m => (a -> m) -> Default1 t a -> m
#endif
  gfoldMap f (Default1 tx) = gfoldMapdefault f tx
