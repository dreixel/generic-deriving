{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

module Generics.Deriving.Traversable (
  -- * GTraversable class
    GTraversable(..)

  -- * Default method
  , gtraversedefault

  ) where

import Control.Applicative
import Generics.Deriving.Base
import Generics.Deriving.Foldable
import Generics.Deriving.Functor
import Generics.Deriving.Instances ()

--------------------------------------------------------------------------------
-- Generic traverse
--------------------------------------------------------------------------------

class GTraversable' t where
  gtraverse' :: Applicative f => (a -> f b) -> t a -> f (t b)

instance GTraversable' U1 where
  gtraverse' _ U1 = pure U1

instance GTraversable' Par1 where
  gtraverse' f (Par1 a) = Par1 <$> f a

instance GTraversable' (K1 i c) where
  gtraverse' _ (K1 a) = pure (K1 a)

instance (GTraversable f) => GTraversable' (Rec1 f) where
  gtraverse' f (Rec1 a) = Rec1 <$> gtraverse f a

instance (GTraversable' f) => GTraversable' (M1 i c f) where
  gtraverse' f (M1 a) = M1 <$> gtraverse' f a

instance (GTraversable' f, GTraversable' g) => GTraversable' (f :+: g) where
  gtraverse' f (L1 a) = L1 <$> gtraverse' f a
  gtraverse' f (R1 a) = R1 <$> gtraverse' f a

instance (GTraversable' f, GTraversable' g) => GTraversable' (f :*: g) where
  gtraverse' f (a :*: b) = (:*:) <$> gtraverse' f a <*> gtraverse' f b

instance (GTraversable f, GTraversable' g) => GTraversable' (f :.: g) where
  gtraverse' f (Comp1 x) = Comp1 <$> gtraverse (gtraverse' f) x


class (GFunctor t, GFoldable t) => GTraversable t where
  gtraverse :: Applicative f => (a -> f b) -> t a -> f (t b)
#if __GLASGOW_HASKELL__ >= 701
  default gtraverse :: (Generic1 t, GTraversable' (Rep1 t), Applicative f)
                    => (a -> f b) -> t a -> f (t b)
  gtraverse = gtraversedefault
#endif

  gsequenceA :: Applicative f => t (f a) -> f (t a)
  gsequenceA = gtraverse id

  gmapM :: Monad m => (a -> m b) -> t a -> m (t b)
  gmapM f = unwrapMonad . gtraverse (WrapMonad . f)

  gsequence :: Monad m => t (m a) -> m (t a)
  gsequence = gmapM id

gtraversedefault :: (Generic1 t, GTraversable' (Rep1 t), Applicative f)
                 => (a -> f b) -> t a -> f (t b)
gtraversedefault f x = to1 <$> gtraverse' f (from1 x)

-- Base types instances
instance GTraversable Maybe where
  gtraverse = gtraversedefault

instance GTraversable [] where
  gtraverse = gtraversedefault
