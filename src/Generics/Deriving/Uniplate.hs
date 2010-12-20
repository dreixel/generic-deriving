{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE IncoherentInstances #-} -- :-/
{-# LANGUAGE CPP #-}

module Generics.Deriving.Uniplate (
    Uniplate(..)

  -- * Default definition
  , childrendefault

  ) where


import Generics.Deriving.Base
import Generics.Deriving.Instances

--------------------------------------------------------------------------------
-- Generic Uniplate
--------------------------------------------------------------------------------

class Uniplate' f b where
  children' :: f a -> [b]

instance Uniplate' U1 a where
  children' U1 = []

instance Uniplate' (K1 i a) a where
  children' (K1 a) = [a]

instance Uniplate' (K1 i a) b where
  children' (K1 _) = []

instance (Uniplate' f b) => Uniplate' (M1 i c f) b where
  children' (M1 a) = children' a

instance (Uniplate' f b, Uniplate' g b) => Uniplate' (f :+: g) b where
  children' (L1 a) = children' a
  children' (R1 a) = children' a

instance (Uniplate' f b, Uniplate' g b) => Uniplate' (f :*: g) b where
  children' (a :*: b) = children' a ++ children' b 


class Uniplate a where 
  children :: a -> [a]
  children _ = []

#ifdef __UHC__

{-# DERIVABLE Uniplate children childrendefault #-}
deriving instance (Uniplate a) => Uniplate (Maybe a)

#endif

childrendefault :: (Representable0 a rep0, Uniplate' rep0 a) => rep0 x -> a -> [a]
childrendefault rep x = children' (from0 x `asTypeOf` rep)


-- Base types instances
instance Uniplate Char
instance Uniplate Int
instance Uniplate Float

instance Uniplate [a] where
  children []    = []
  children (_:t) = [t]

#ifndef __UHC__
instance (Uniplate a) => Uniplate (Maybe a) where
  children = t undefined where
    t :: Rep0Maybe a x -> Maybe a -> [Maybe a]
    t = childrendefault
#endif
