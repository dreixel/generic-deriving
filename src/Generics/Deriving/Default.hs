-- |
-- Module      : Generics.Deriving.Default
-- Description : Default implementations of generic classes
-- License     : BSD-3-Clause
--
-- Maintainer  : generics@haskell.org
-- Stability   : experimental
-- Portability : non-portable
--
-- GHC 8.6 introduced the
-- @<https://downloads.haskell.org/~ghc/8.6.3/docs/html/users_guide/glasgow_exts.html?highlight=derivingvia#extension-DerivingVia DerivingVia>@
-- language extension, which means a typeclass instance can be derived from
-- an existing instance for an isomorphic type. Any newtype is isomorphic
-- to the underlying type. By implementing a typeclass once for the newtype,
-- it is possible to derive any typeclass for any type with a 'Generic' instance.
--
-- For a number of classes, there are sensible default instantiations. In
-- older GHCs, these can be supplied in the class definition, using the
-- @<https://downloads.haskell.org/~ghc/8.6.3/docs/html/users_guide/glasgow_exts.html?highlight=defaultsignatures#extension-DefaultSignatures DefaultSignatures>@
-- extension. However, only one default can be provided! With
-- @<https://downloads.haskell.org/~ghc/8.6.3/docs/html/users_guide/glasgow_exts.html?highlight=derivingvia#extension-DerivingVia DerivingVia>@
-- it is now possible to choose from many
-- default instantiations.
--
-- This package contains a number of such classes. This module demonstrates
-- how one might create a family of newtypes ('Default', 'Default1') for
-- which such instances are defined.
--
-- One might then use
-- @<https://downloads.haskell.org/~ghc/8.6.3/docs/html/users_guide/glasgow_exts.html?highlight=derivingvia#extension-DerivingVia DerivingVia>@
-- as follows. The implementations of the data types are elided here (they
-- are irrelevant). For most cases, either the deriving clause with the
-- data type definition or the standalone clause will work (for some types
-- it is necessary to supply the context explicitly using the latter form).
-- See the source of this module for the implementations of instances for
-- the 'Default' family of newtypes and the source of the test suite for
-- some types which derive instances via these wrappers.

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
# if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
# else
{-# LANGUAGE Trustworthy #-}
# endif
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Deriving.Default
  ( -- * Kind @*@ (aka @Type@)

    -- $default

    Default(..)

  , -- * Kind @* -> *@ (aka @Type -> Type@)

    -- $default1

    Default1(..)

    -- * Other kinds

    -- $other-kinds
  ) where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<$>))
#endif
import Control.Monad (liftM)

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

-- $default
--
-- For classes which take an argument of kind 'Data.Kind.Type', use
-- 'Default'. An example of this class from @base@ would be 'Eq', or
-- 'Generic'.
--
-- These examples use 'GShow' and 'GEq'; they are interchangeable.
--
-- @
-- data MyType = …
--  deriving ('Generic')
--  deriving ('GEq') via ('Default' MyType)
--
-- deriving via ('Default' MyType) instance 'GShow' MyType
-- @
--
-- Instances may be parameterized by type variables.
--
-- @
-- data MyType1 a = …
--  deriving ('Generic')
--  deriving ('GShow') via ('Default' (MyType1 a))
--
-- deriving via 'Default' (MyType1 a) instance 'GEq' a => 'GEq' (MyType1 a)
-- @
--
-- These types both require instances for 'Generic'. This is because the
-- implementations of 'geq' and 'gshowsPrec' for @'Default' b@ have a @'Generic'
-- b@ constraint, i.e. the type corresponding to @b@ require a 'Generic'
-- instance. For these two types, that means instances for @'Generic' MyType@
-- and @'Generic' (MyType1 a)@ respectively.
--
-- It also means the 'Generic' instance is not needed when there is already
-- a generic instance for the type used to derive the relevant instances.
-- For an example, see the documentation of the 'GShow' instance for
-- 'Default', below.

-- | This newtype wrapper can be used to derive default instances for
-- classes taking an argument of kind 'Data.Kind.Type'.
newtype Default a = Default { unDefault :: a }

-- $default1
--
-- For classes which take an argument of kind @'Data.Kind.Type' ->
-- 'Data.Kind.Type'@, use 'Default1'.  An example of this class from @base@
-- would be 'Data.Functor.Classes.Eq1', or 'Generic1'.
--
-- Unlike for @MyType1@, there can be no implementation of these classes for @MyType :: 'Data.Kind.Type'@.
--
-- @
-- data MyType1 a = …
--  deriving ('Generic1')
--  deriving ('GFunctor') via ('Default1' MyType1)
--
-- deriving via ('Default1' MyType1) instance 'GFoldable' MyType1
-- @
--
-- Note that these instances require a @'Generic1' MyType1@ constraint as
-- 'gmap' and 'gfoldMap' have @'Generic1' a@ constraints on the
-- implementations for @'Default1' a@.

-- | This newtype wrapper can be used to derive default instances for
-- classes taking an argument of kind @'Data.Kind.Type' -> 'Data.Kind.Type'@.
newtype Default1 f a = Default1 { unDefault1 :: f a }

-- $other-kinds
--
-- These principles extend to classes taking arguments of other kinds.

--------------------------------------------------------------------------------
-- Eq
--------------------------------------------------------------------------------

instance (Generic a, GEq' (Rep a)) => GEq (Default a) where
  -- geq :: Default a -> Default a -> Bool
  Default x `geq` Default y = x `geqdefault` y

--------------------------------------------------------------------------------
-- Enum
--------------------------------------------------------------------------------

-- | The 'Enum' class in @base@ is slightly different; it comprises 'toEnum' and
-- 'fromEnum'. "Generics.Deriving.Enum" provides functions 'toEnumDefault'
-- and 'fromEnumDefault'.
instance (Generic a, GEq a, Enum' (Rep a)) => GEnum (Default a) where
  -- genum :: [Default a]
  genum = Default . to <$> enum'

--------------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------------

-- | For example, with this type:
--
-- @
-- newtype TestShow = TestShow 'Bool'
--   deriving ('GShow') via ('Default' 'Bool')
-- @
--
-- 'gshow' for @TestShow@ would produce the same string as `gshow` for
-- 'Bool'.
--
-- In this example, @TestShow@ requires no 'Generic' instance, as the
-- constraint on 'gshowsPrec' from @'Default' 'Bool'@ is @'Generic' 'Bool'@.
--
-- In general, when using a newtype wrapper, the instance can be derived
-- via the wrapped type, as here (via @'Default' 'Bool'@ rather than @'Default'
-- TestShow@).
instance (Generic a, GShow' (Rep a)) => GShow (Default a) where
  -- gshowsPrec :: Int -> Default a -> ShowS
  gshowsPrec n (Default x) = gshowsPrecdefault n x

--------------------------------------------------------------------------------
-- Semigroup
--------------------------------------------------------------------------------

-- | Semigroups often have many sensible implementations of
-- 'Data.Semigroup.<>' / 'gsappend', and therefore no sensible default.
-- Indeed, there is no 'GSemigroup'' instance for representations of sum
-- types.
--
-- In other cases, one may wish to use the existing wrapper newtypes in
-- @base@, such as the following (using 'Data.Semigroup.First'):
--
-- @
-- newtype FirstSemigroup = FirstSemigroup 'Bool'
--   deriving stock ('Eq', 'Show')
--   deriving ('GSemigroup') via ('Data.Semigroup.First' 'Bool')
-- @
--
instance (Generic a, GSemigroup' (Rep a)) => GSemigroup (Default a) where
  -- gsappend :: Default a -> Default a -> Default a
  Default x `gsappend` Default y = Default $ x `gsappenddefault` y

--------------------------------------------------------------------------------
-- Monoid
--------------------------------------------------------------------------------

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

  children     (Default x)    =       Default <$> childrendefault    x
  context      (Default x) ys =       Default  $  contextdefault     x    (unDefault <$> ys)
  descend    f (Default x)    =       Default  $  descenddefault          (unDefault . f . Default) x
  descendM   f (Default x)    = liftM Default  $  descendMdefault   (liftM unDefault . f . Default) x
  transform  f (Default x)    =       Default  $  transformdefault        (unDefault . f . Default) x
  transformM f (Default x)    = liftM Default  $  transformMdefault (liftM unDefault . f . Default) x

--------------------------------------------------------------------------------
-- Functor
--------------------------------------------------------------------------------

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

instance (Generic1 t, GFoldable' (Rep1 t)) => GFoldable (Default1 t) where
  -- gfoldMap :: Monoid m => (a -> m) -> Default1 t a -> m
  gfoldMap f (Default1 tx) = gfoldMapdefault f tx

--------------------------------------------------
-- Traversable
--------------------------------------------------

instance (Generic1 t, GFunctor' (Rep1 t), GFoldable' (Rep1 t), GTraversable' (Rep1 t)) => GTraversable (Default1 t) where
  -- gtraverse :: Applicative f => (a -> f b) -> Default1 t a -> f (Default1 t b)
  gtraverse f (Default1 fx) = Default1 <$> gtraversedefault f fx
