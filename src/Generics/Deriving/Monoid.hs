{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Generics.Deriving.Monoid (module Generics.Deriving.Monoid.Internal) where

import Generics.Deriving.Monoid.Internal
import Generics.Deriving.Semigroup (GSemigroup(..))

instance GSemigroup a => GMonoid (Maybe a) where
  gmempty = Nothing
  gmappend = gsappend
