{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

module Generics.Deriving.Functor (
  -- * GFunctor class
    GFunctor(..)

  -- * Default method
  , gmapdefault

  ) where

import Generics.Deriving.Base

--------------------------------------------------------------------------------
-- Generic fmap
--------------------------------------------------------------------------------

class GFunctor' f where
  gmap' :: (a -> b) -> f a -> f b

instance GFunctor' U1 where
  gmap' _ U1 = U1

instance GFunctor' Par1 where
  gmap' f (Par1 a) = Par1 (f a)

instance GFunctor' (K1 i c) where
  gmap' _ (K1 a) = K1 a

instance (GFunctor f) => GFunctor' (Rec1 f) where
  gmap' f (Rec1 a) = Rec1 (gmap f a)

instance (GFunctor' f) => GFunctor' (M1 i c f) where
  gmap' f (M1 a) = M1 (gmap' f a)

instance (GFunctor' f, GFunctor' g) => GFunctor' (f :+: g) where
  gmap' f (L1 a) = L1 (gmap' f a)
  gmap' f (R1 a) = R1 (gmap' f a)

instance (GFunctor' f, GFunctor' g) => GFunctor' (f :*: g) where
  gmap' f (a :*: b) = gmap' f a :*: gmap' f b

instance (GFunctor f, GFunctor' g) => GFunctor' (f :.: g) where
  gmap' f (Comp1 x) = Comp1 (gmap (gmap' f) x)


class GFunctor f where
  gmap :: (a -> b) -> f a -> f b

gmapdefault :: (Representable1 f rep, GFunctor' rep)
            => rep a -> (a -> b) -> f a -> f b
gmapdefault ra f x = to1 (gmap' f (from1 x `asTypeOf` ra))

#ifdef __UHC__

{-# DERIVABLE GFunctor gmap gmapdefault #-}

deriving instance GFunctor Maybe
deriving instance GFunctor []

#else

-- Base types instances
instance GFunctor Maybe where
  gmap = t undefined where
    t :: Rep1Maybe a -> (a -> b) -> Maybe a -> Maybe b
    t = gmapdefault

instance GFunctor [] where
  gmap = t undefined where
    t :: Rep1List_ a -> (a -> b) -> [a] -> [b]
    t = gmapdefault

#endif
