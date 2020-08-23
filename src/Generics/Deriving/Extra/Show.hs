{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Deriving.Extra.Show
  ( module Generics.Deriving.Show
  ) where

import                    Generics.Deriving.Extra.Base
import "generic-deriving" Generics.Deriving.Show

instance (c => GShow' f) => GShow' (c :=>: f) where
  gshowsPrec' t n (SuchThat a) = gshowsPrec' t n a
  isNullary (SuchThat x) = isNullary x
