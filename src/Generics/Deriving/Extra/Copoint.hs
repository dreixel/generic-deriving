{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Deriving.Extra.Copoint
  ( module Generics.Deriving.Copoint
  ) where

import "generic-deriving" Generics.Deriving.Copoint
import                    Generics.Deriving.Extra.Base

instance (c => GCopoint' f) => GCopoint' (c :=>: f) where
    gcopoint' (SuchThat x) = gcopoint' x
