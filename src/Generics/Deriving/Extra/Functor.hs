{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Deriving.Extra.Functor
  ( module Generics.Deriving.Functor
  ) where

import                    Generics.Deriving.Extra.Base
import "generic-deriving" Generics.Deriving.Functor

instance (c => GFunctor' f) => GFunctor' (c :=>: f) where
  gmap' f (SuchThat x) = SuchThat (gmap' f x)
