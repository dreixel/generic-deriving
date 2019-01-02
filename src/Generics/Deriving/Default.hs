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
-- a typeclass instance can be derived from an existing instance for an
-- isomorphic type. Any newtype is isomorphic to the underlying type. By
-- implementing a typeclass once for the newtype, it is possible to derive
-- any typeclass for any type with a 'Generic' instance.
--
-- For a number of classes, there are sensible default instantiations. In
-- older GHCs, these can be supplied in the class definition, using the
-- 'DefaultSignatures' extension. However, only one default can be
-- provided! With 'DerivingVia' it is now possible to choose from many
-- default instantiations.
--
-- This package contains a number of such classes. This module demonstrates
-- how one might create a newtype called 'Default' for which such instances
-- are defined.
--
-- Then one might use 'DerivingVia' as follows. The implementations of the
-- data types are elided here (they are irrelevant). Either the deriving
-- clause with the data type definition or the standalone clause will work.
--
-- 1.  For data types of kind '*', use 'Default'.
--
-- @
-- data MyType = …
--  deriving (Generic)
--  deriving (GEq) via (Default MyType)
--
-- deriving via (Default MyType) instance GShow MyType
-- @
--
-- TODO Confirm classes of kind '* -> Constraint' vs kind 'Constraint'
--
-- 2.  For data types of kind '* -> *', use 'Default1'.
--
-- @
-- data MyType1 a = …
--  deriving (Generic)
--  deriving (GFunctor) via (Default1 MyType1)
--
-- deriving via (Default1 MyType1) instance GFoldable MyType1
-- @

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Deriving.Default
  ( Default(..)
  , Default1(..)
  ) where

import Generics.Deriving.Base
import Generics.Deriving.Copoint
import Generics.Deriving.Enum
import Generics.Deriving.Eq
import Generics.Deriving.Foldable
import Generics.Deriving.Functor
import Generics.Deriving.Monoid
import Generics.Deriving.Semigroup
import Generics.Deriving.Show
import Generics.Deriving.Traversable
import Generics.Deriving.Uniplate

-- | This newtype wrapper can be used to derive default instances for data
-- types of kind '*'.
newtype Default a = Default { unDefault :: a }

-- | This newtype wrapper can be used to derive default instances for data
-- types of kind '* -> *'.
newtype Default1 f a = Default1 { unDefault1 :: f a }

--------------------------------------------------------------------------------
-- Eq
--------------------------------------------------------------------------------

-- |
--
-- @
-- instance (Generic a, GEq' (Rep a)) => Eq (Default a) where
--   (==) :: Default a -> Default a -> Bool
--   Default x == Default y = x `geqdefault` y
-- @
--
instance (Generic a, GEq' (Rep a)) => GEq (Default a) where
  -- geq :: Default a -> Default a -> Bool
  Default x `geq` Default y = x `geqdefault` y

--------------------------------------------------------------------------------
-- Enum
--------------------------------------------------------------------------------

-- |
--
-- @
-- instance (Generic a, GEq a, Enum' (Rep a)) => Enum (Default a) where
--   toEnum :: Int -> Default a
--   toEnum = Default . toEnumDefault
--
--   fromEnum :: Default a -> Int
--   fromEnum (Default x) = fromEnumDefault x
-- @
--
instance (Generic a, GEq a, Enum' (Rep a)) => GEnum (Default a) where
  -- genum :: [Default a]
  genum = Default . to <$> enum'

--------------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------------

-- |
--
-- @
-- instance (Generic a, GShow' (Rep a)) => Show (Default a) where
--   showsPrec :: Int -> Default a -> ShowS
--   showsPrec = gshowsPrec
-- @
--
instance (Generic a, GShow' (Rep a)) => GShow (Default a) where
  -- gshowsPrec :: Int -> Default a -> ShowS
  gshowsPrec n (Default x) = gshowsPrecdefault n x

--------------------------------------------------------------------------------
-- Semigroup
--------------------------------------------------------------------------------

-- |
--
-- @
-- instance (Generic a, GSemigroup' (Rep a)) => Semigroup (Default a) where
--   (<>) :: Default a -> Default a -> Default a
--   (<>) = gsappend
-- @
--
instance (Generic a, GSemigroup' (Rep a)) => GSemigroup (Default a) where
  -- gsappend :: Default a -> Default a -> Default a
  Default x `gsappend` Default y = Default $ x `gsappenddefault` y

--------------------------------------------------------------------------------
-- Monoid
--------------------------------------------------------------------------------

-- |
--
-- @
-- #if (MIN_VERSION_base(4,11,0))
-- instance (Generic a, GMonoid' (Rep a), Semigroup a, GSemigroup' (Rep a)) => Monoid (Default a) where
-- #else
-- instance (Generic a, GMonoid' (Rep a)) => Monoid (Default a) where
-- #endif
--
--   mempty :: Default a
--   mempty = gmempty
--
--   mappend :: Default a -> Default a -> Default a
--   mappend = gmappend
-- @
--
instance (Generic a, GMonoid' (Rep a)) => GMonoid (Default a) where
  -- gmempty :: Default a
  gmempty = Default gmemptydefault

  -- gmappend :: Default a -> Default a -> Default a
  Default x `gmappend` Default y = Default $ x `gmappenddefault` y

--------------------------------------------------------------------------------
-- Uniplate
--------------------------------------------------------------------------------

instance (Generic a, Uniplate' (Rep a) a, Context' (Rep a) a) => Uniplate (Default a) where

  -- children   ::                                             Default a  ->   [Default a]
  -- context    ::                             Default a   -> [Default a] ->    Default a
  -- descend    ::            (Default a ->    Default a)  ->  Default a  ->    Default a
  -- descendM   :: Monad m => (Default a -> m (Default a)) ->  Default a  -> m (Default a)
  -- transform  ::            (Default a ->    Default a)  ->  Default a  ->    Default a
  -- transformM :: Monad m => (Default a -> m (Default a)) ->  Default a  -> m (Default a)

  children     (Default x)    = Default <$> childrendefault    x
  context      (Default x) ys = Default  $  contextdefault     x   (unDefault <$> ys)
  descend    f (Default x)    = Default  $  descenddefault         (unDefault . f . Default) x
  descendM   f (Default x)    = Default <$> descendMdefault   (fmap unDefault . f . Default) x
  transform  f (Default x)    = Default  $  transformdefault       (unDefault . f . Default) x
  transformM f (Default x)    = Default <$> transformMdefault (fmap unDefault . f . Default) x

--------------------------------------------------------------------------------
-- Functor
--------------------------------------------------------------------------------

-- |
--
-- @
-- instance (Generic1 f, GFunctor' (Rep1 f)) => Functor (Default1 f) where
--   fmap = gmap
-- @
--
instance (Generic1 f, GFunctor' (Rep1 f)) => GFunctor (Default1 f) where
  -- gmap :: (a -> b) -> (Default1 f) a -> (Default1 f) b
  gmap f (Default1 fx) = Default1 $ gmapdefault f fx

--------------------------------------------------
-- Copoint
--------------------------------------------------

instance (Generic1 f, GCopoint' (Rep1 f)) => GCopoint (Default1 f) where
  -- gcopoint :: Default1 f a -> a
  gcopoint = gcopointdefault . unDefault1

--------------------------------------------------
-- Foldable
--------------------------------------------------

-- |
--
-- @
-- instance (Generic1 t, GFoldable' (Rep1 t)) => Foldable (Default1 t) where
--
--   foldMap :: (Monoid m) => (a -> m) -> Default1 t a -> m
--   fold    ::  Monoid m              => Default1 t m -> m
--   foldr   :: (a -> b -> b)    -> b  -> Default1 t a -> b
--   foldr'  :: (a -> b -> b)    -> b  -> Default1 t a -> b
--   foldl   :: (a -> b -> a)    -> a  -> Default1 t b -> a
--   foldl'  :: (a -> b -> a)    -> a  -> Default1 t b -> a
--   foldr1  :: (a -> a -> a)          -> Default1 t a -> a
--   foldl1  :: (a -> a -> a)          -> Default1 t a -> a
--
--   foldMap = gfoldMap
--   fold    = gfold
--   foldr   = gfoldr
--   foldr'  = gfoldr'
--   foldl   = gfoldl
--   foldl'  = gfoldl'
--   foldr1  = gfoldr1
--   foldl1  = gfoldl1
-- @
--
instance (Generic1 t, GFoldable' (Rep1 t)) => GFoldable (Default1 t) where
  -- gfoldMap :: Monoid m => (a -> m) -> Default1 t a -> m
  gfoldMap f (Default1 tx) = gfoldMapdefault f tx

--------------------------------------------------
-- Traversable
--------------------------------------------------

-- |
--
-- @
-- instance (Generic1 t, GFunctor' (Rep1 t), GFoldable' (Rep1 t), GTraversable' (Rep1 t)) => Traversable (Default1 t) where
--
--   traverse  :: Applicative f => (a -> f b) -> Default1 t    a  -> f (Default1 t b)
--   sequenceA :: Applicative f =>               Default1 t (f a) -> f (Default1 t a)
--   mapM      ::       Monad m => (a -> m b) -> Default1 t    a  -> m (Default1 t b)
--   sequence  ::       Monad m =>               Default1 t (m a) -> m (Default1 t a)
--
--   sequenceA = gsequenceA
--   mapM      = gmapM
--   sequence  = gsequence
--   traverse  = gtraverse
-- @
--
instance (Generic1 t, GFunctor' (Rep1 t), GFoldable' (Rep1 t), GTraversable' (Rep1 t)) => GTraversable (Default1 t) where
  -- gtraverse :: Applicative f => (a -> f b) -> Default1 t a -> f (Default1 t b)
  gtraverse f (Default1 fx) = Default1 <$> gtraversedefault f fx
