{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Deriving.Semigroup (module Generics.Deriving.Semigroup.Internal) where

import Data.Semigroup (WrappedMonoid(..))

import Generics.Deriving.Monoid.Internal (GMonoid(..))
import Generics.Deriving.Semigroup.Internal

instance GMonoid m => GSemigroup (WrappedMonoid m) where
  gsappend (WrapMonoid a) (WrapMonoid b) = WrapMonoid (gmappend a b)
