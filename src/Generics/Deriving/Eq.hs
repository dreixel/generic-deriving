{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

module Generics.Deriving.Eq (
  -- * Generic show class
    GEq(..)

  -- * Default definition
  , geqdefault

  ) where


import Generics.Deriving.Base
import Generics.Deriving.Instances

--------------------------------------------------------------------------------
-- Generic show
--------------------------------------------------------------------------------

class GEq' f where
  geq' :: f a -> f a -> Bool

instance GEq' U1 where
  geq' _ _ = True

instance (GEq c) => GEq' (K1 i c) where
  geq' (K1 a) (K1 b) = geq a b

-- No instances for P or Rec because geq is only applicable to types of kind *

instance (GEq' a) => GEq' (M1 i c a) where
  geq' (M1 a) (M1 b) = geq' a b

instance (GEq' a, GEq' b) => GEq' (a :+: b) where
  geq' (L1 a) (L1 b) = geq' a b
  geq' (R1 a) (R1 b) = geq' a b
  geq' _      _      = False

instance (GEq' a, GEq' b) => GEq' (a :*: b) where
  geq' (a1 :*: b1) (a2 :*: b2) = geq' a1 a2 && geq' b1 b2


class GEq a where 
  geq :: a -> a -> Bool

#ifdef __UHC__

{-# DERIVABLE GEq geq geqdefault #-}
deriving instance (GEq a) => GEq (Maybe a)
deriving instance (GEq a) => GEq [a]

#endif

geqdefault :: (Representable0 a rep0, GEq' rep0) => rep0 x -> a -> a -> Bool
geqdefault rep x y = geq' (from0 x `asTypeOf` rep) (from0 y `asTypeOf` rep)


-- Base types instances
instance GEq Char   where geq = (==)
instance GEq Int    where geq = (==)
instance GEq Float  where geq = (==)


#ifndef __UHC__

instance (GEq a) => GEq (Maybe a) where
  geq = t undefined where
    t :: (GEq a) => Rep0Maybe a x -> Maybe a -> Maybe a -> Bool
    t = geqdefault

instance (GEq a) => GEq [a] where
  geq = t undefined where
    t :: (GEq a) => Rep0List a x -> [a] -> [a] -> Bool
    t = geqdefault

#endif
