{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generics.Deriving.Monoid (module Generics.Deriving.Monoid.Internal) where

import Generics.Deriving.Monoid.Internal
import Generics.Deriving.Semigroup (GSemigroup(..))

instance GSemigroup a => GMonoid (Maybe a) where
  gmempty = Nothing
  gmappend = gsappend
