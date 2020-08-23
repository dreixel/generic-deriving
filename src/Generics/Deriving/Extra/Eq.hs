{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Deriving.Extra.Eq
  ( module Generics.Deriving.Eq
  ) where

import "generic-deriving" Generics.Deriving.Eq
import                    Generics.Deriving.Extra.Base

instance (c => GEq' f) => GEq' (c :=>: f) where
  geq' (SuchThat x) (SuchThat y) = geq' x y
