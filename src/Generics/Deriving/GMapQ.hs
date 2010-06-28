{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}

module Generics.Deriving.GMapQ (
  -- * Generic show class
    GMapQ(..)

  -- * Default definition
  , gmapQdefault

  ) where


import Generics.Deriving.Base

-- Does not work

--------------------------------------------------------------------------------
-- gmapQ
--------------------------------------------------------------------------------

class GMapQ' f where
  gmapQ' :: (forall d. Representable0 d rep0 => d -> u) -> f a -> [u]

instance GMapQ' U1 where
  gmapQ' f U1 = [f U1]

instance (GMapQ c) => GMapQ' (K1 i c) where
  gmapQ' f (K1 a) = gmapQ f a

instance (GMapQ' a) => GMapQ' (M1 i c a) where
  gmapQ' f (M1 a) = gmapQ' f a

instance (GMapQ' a, GMapQ' b) => GMapQ' (a :+: b) where
  gmapQ' f (L1 a) = gmapQ' f a
  gmapQ' f (R1 a) = gmapQ' f a

instance (GMapQ' a, GMapQ' b) => GMapQ' (a :*: b) where
  gmapQ' f (a :*: b) = gmapQ' f a ++ gmapQ' f b


gmapQdefault :: (Representable0 a rep0, GMapQ' rep0)
             => rep0 x -> (forall d. Representable0 d rep0' => d -> u) -> a -> [u]
gmapQdefault rep f x = gmapQ' f (from0 x `asTypeOf` rep)


#ifdef __UHC__

{-# DERIVABLE GMapQ gmapQ gmapQdefault #-}
deriving instance (GMapQ a) => GMapQ (Maybe a)
deriving instance (GMapQ a) => GMapQ [a]

#endif


class GMapQ a where
  gmapQ :: (forall d. Representable0 d rep0 => d -> u) -> a -> [u]
  gmapQ f a = [f a]

-- Base types instances
instance GMapQ Char
instance GMapQ Int
instance GMapQ Float


#ifndef __UHC__

instance (GMapQ a) => GMapQ (Maybe a) where
  gmapQ = t undefined where
    t :: (GMapQ a) => Rep0Maybe a x -> (forall d. Representable0 d rep0 => d -> u) -> Maybe a -> [u]
    t = gmapQdefault

#endif

