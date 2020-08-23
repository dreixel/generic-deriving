-- |
-- Module      : Generics.Deriving.Extra.Default
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

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Safe #-}

module Generics.Deriving.Extra.Default
  ( module Generics.Deriving.Default
  ) where

import "generic-deriving" Generics.Deriving.Default
