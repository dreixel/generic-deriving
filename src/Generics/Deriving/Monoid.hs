{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

module Generics.Deriving.Monoid (module Generics.Deriving.Monoid.Internal) where

import Generics.Deriving.Monoid.Internal
import Generics.Deriving.Semigroup (GSemigroup(..))

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (WrappedMonoid)
#endif

instance GSemigroup a => GMonoid (Maybe a) where
  gmempty = Nothing
  gmappend = gsappend

#if MIN_VERSION_base(4,9,0)
instance GMonoid m => GMonoid (WrappedMonoid m) where
  gmempty  = gmemptydefault
  gmappend = gmappenddefault
#endif
