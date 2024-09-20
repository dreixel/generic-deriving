{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}

module Generics.Deriving.Copoint (
  -- * GCopoint class
    GCopoint(..)

  -- * Default method
  , gcopointdefault

  -- * Internal class
  , GCopoint'(..)

  ) where

import           Control.Applicative (WrappedMonad)

import           Data.Functor.Identity (Identity)
import qualified Data.Functor.Sum as Functor (Sum)
import           Data.Monoid (Alt, Dual)
import qualified Data.Monoid as Monoid (Sum)
import           Data.Ord (Down)
import           Data.Semigroup (Arg, First, Last, Max, Min, WrappedMonoid)

import           Generics.Deriving.Base

--------------------------------------------------------------------------------
-- Generic copoint
--------------------------------------------------------------------------------

-- General copoint may return 'Nothing'

class GCopoint' t where
    gcopoint' :: t a -> Maybe a

instance GCopoint' V1 where
    gcopoint' _ = Nothing

instance GCopoint' U1 where
    gcopoint' U1 = Nothing

instance GCopoint' Par1 where
    gcopoint' (Par1 a) = Just a

instance GCopoint' (K1 i c) where
    gcopoint' _ = Nothing

instance GCopoint' f => GCopoint' (M1 i c f) where
    gcopoint' (M1 a) = gcopoint' a

instance (GCopoint' f, GCopoint' g) => GCopoint' (f :+: g) where
    gcopoint' (L1 a) = gcopoint' a
    gcopoint' (R1 a) = gcopoint' a

-- Favours left "hole" for copoint
instance (GCopoint' f, GCopoint' g) => GCopoint' (f :*: g) where
    gcopoint' (a :*: b) = case (gcopoint' a) of
                            Just x -> Just x
                            Nothing -> gcopoint' b

instance (GCopoint f) => GCopoint' (Rec1 f) where
    gcopoint' (Rec1 a) = Just $ gcopoint a

instance (GCopoint f, GCopoint' g) => GCopoint' (f :.: g) where
    gcopoint' (Comp1 x) = gcopoint' . gcopoint $ x

class GCopoint d where
  gcopoint :: d a -> a
  default gcopoint :: (Generic1 d, GCopoint' (Rep1 d))
                   => (d a -> a)
  gcopoint = gcopointdefault

gcopointdefault :: (Generic1 d, GCopoint' (Rep1 d))
                => d a -> a
gcopointdefault x = case (gcopoint' . from1 $ x) of
                      Just x' -> x'
                      Nothing -> error "Data type is not copointed"

-- instance (Generic1 d, GCopoint' (Rep1 d)) => GCopoint d

-- Base types instances
instance GCopoint ((,) a) where
  gcopoint = gcopointdefault

instance GCopoint ((,,) a b) where
  gcopoint = gcopointdefault

instance GCopoint ((,,,) a b c) where
  gcopoint = gcopointdefault

instance GCopoint ((,,,,) a b c d) where
  gcopoint = gcopointdefault

instance GCopoint ((,,,,,) a b c d e) where
  gcopoint = gcopointdefault

instance GCopoint ((,,,,,,) a b c d e f) where
  gcopoint = gcopointdefault

instance GCopoint f => GCopoint (Alt f) where
  gcopoint = gcopointdefault

instance GCopoint (Arg a) where
  gcopoint = gcopointdefault

instance GCopoint Down where
  gcopoint = gcopointdefault

instance GCopoint Dual where
  gcopoint = gcopointdefault

instance GCopoint First where
  gcopoint = gcopointdefault

instance GCopoint Identity where
  gcopoint = gcopointdefault

instance GCopoint Last where
  gcopoint = gcopointdefault

instance GCopoint Max where
  gcopoint = gcopointdefault

instance GCopoint Min where
  gcopoint = gcopointdefault

instance (GCopoint f, GCopoint g) => GCopoint (Functor.Sum f g) where
  gcopoint = gcopointdefault

instance GCopoint Monoid.Sum where
  gcopoint = gcopointdefault

instance GCopoint m => GCopoint (WrappedMonad m) where
  gcopoint = gcopointdefault

instance GCopoint WrappedMonoid where
  gcopoint = gcopointdefault
