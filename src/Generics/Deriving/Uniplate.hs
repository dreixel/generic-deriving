{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

module Generics.Deriving.Uniplate (
    Uniplate(..)

  -- * Default definition
  , childrendefault

  ) where


import Generics.Deriving.Base
import Generics.Deriving.Instances ()

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
#if __GLASGOW_HASKELL__ >= 701
  default children :: (Generic a, Uniplate' (Rep a) a) => a -> [a]
  children = childrendefault
#endif

childrendefault :: (Generic a, Uniplate' (Rep a) a) => a -> [a]
childrendefault = children' . from


-- Base types instances
instance Uniplate Bool   where children _ = []
instance Uniplate Char   where children _ = []
instance Uniplate Double where children _ = []
instance Uniplate Float  where children _ = []
instance Uniplate Int    where children _ = []
instance Uniplate ()     where children _ = []

-- Tuple instances
instance Uniplate (b,c)           where children _ = []
instance Uniplate (b,c,d)         where children _ = []
instance Uniplate (b,c,d,e)       where children _ = []
instance Uniplate (b,c,d,e,f)     where children _ = []
instance Uniplate (b,c,d,e,f,g)   where children _ = []
instance Uniplate (b,c,d,e,f,g,h) where children _ = []

-- Parameterized type instances
instance Uniplate (Maybe a)    where children _ = []
instance Uniplate (Either a b) where children _ = []

instance Uniplate [a] where
  children []    = []
  children (_:t) = [t]

