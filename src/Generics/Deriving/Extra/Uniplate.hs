{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      :  Generics.Deriving.Extra.Uniplate
Copyright   :  2011-2012 Universiteit Utrecht, University of Oxford
License     :  BSD3

Maintainer  :  generics@haskell.org
Stability   :  experimental
Portability :  non-portable

Summary: Functions inspired by the Uniplate generic programming library,
mostly implemented by Sean Leather.
-}

module Generics.Deriving.Extra.Uniplate
  ( module Generics.Deriving.Uniplate
  ) where


import                    Control.Monad (liftM)

import                    Generics.Deriving.Extra.Base
import "generic-deriving" Generics.Deriving.Uniplate

instance (c => Uniplate' f b) => Uniplate' (c :=>: f) b where
  children' (SuchThat a) = children' a
  descend' f (SuchThat a) = SuchThat (descend' f a)
  descendM' f (SuchThat a) = liftM SuchThat (descendM' f a)
  transform' f (SuchThat a) = SuchThat (transform' f a)
  transformM' f (SuchThat a) = liftM SuchThat (transformM' f a)
