{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Deriving.Extra.Foldable
  ( module Generics.Deriving.Foldable
  ) where

import                    Generics.Deriving.Extra.Base
import "generic-deriving" Generics.Deriving.Foldable

instance (c => GFoldable' f) => GFoldable' (c :=>: f) where
  gfoldMap' f (SuchThat x) = gfoldMap' f x
