{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Deriving.Monoid (module Generics.Deriving.Monoid.Internal) where

import Data.Semigroup (WrappedMonoid)

import Generics.Deriving.Monoid.Internal
import Generics.Deriving.Semigroup (GSemigroup(..))

instance GSemigroup a => GMonoid (Maybe a) where
  gmempty = Nothing
  gmappend = gsappend

instance GMonoid m => GMonoid (WrappedMonoid m) where
  gmempty  = gmemptydefault
  gmappend = gmappenddefault
