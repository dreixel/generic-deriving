{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Deriving.Extra.Traversable
  ( module Generics.Deriving.Traversable
  ) where

import                    Generics.Deriving.Extra.Base
import "generic-deriving" Generics.Deriving.Traversable

instance (c => GTraversable' f) => GTraversable' (c :=>: f) where
  gtraverse' f (SuchThat x) = SuchThat <$> gtraverse' f x
