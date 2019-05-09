{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

module Generics.Deriving.Semigroup (module Generics.Deriving.Semigroup.Internal) where

import Generics.Deriving.Semigroup.Internal

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (WrappedMonoid(..))
import Generics.Deriving.Monoid.Internal (GMonoid(..))

instance GMonoid m => GSemigroup (WrappedMonoid m) where
  gsappend (WrapMonoid a) (WrapMonoid b) = WrapMonoid (gmappend a b)
#endif
