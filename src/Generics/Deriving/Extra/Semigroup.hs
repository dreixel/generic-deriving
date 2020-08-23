{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Deriving.Extra.Semigroup
  ( module Generics.Deriving.Semigroup
  ) where

import                    Generics.Deriving.Extra.Base
import "generic-deriving" Generics.Deriving.Semigroup

instance (c => GSemigroup' f) => GSemigroup' (c :=>: f) where
  gsappend' (SuchThat x) (SuchThat y) = SuchThat (gsappend' x y)
