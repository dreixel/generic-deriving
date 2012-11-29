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

  -- * Derived functions
  , universe
  , rewrite
  , rewriteM
  , para

  -- * Default definitions
  , childrendefault
  , descenddefault
  , descendMdefault
  , transformdefault
  , transformMdefault

  ) where


import Generics.Deriving.Base
import Generics.Deriving.Instances ()

import Control.Monad (liftM, liftM2)
import GHC.Exts (build)

--------------------------------------------------------------------------------
-- Generic Uniplate
--------------------------------------------------------------------------------

class Uniplate' f b where
  children'  :: f a -> [b]
  descend'   :: (b -> b) -> f a -> f a
  descendM'  :: Monad m => (b -> m b) -> f a -> m (f a)
  transform' :: (b -> b) -> f a -> f a
  transformM'  :: Monad m => (b -> m b) -> f a -> m (f a)

instance Uniplate' U1 a where
  children' U1 = []
  descend' _ U1 = U1
  descendM' _ U1 = return U1
  transform' _ U1 = U1
  transformM' _ U1 = return U1

instance (Uniplate a) => Uniplate' (K1 i a) a where
  children' (K1 a) = [a]
  descend' f (K1 a) = K1 (f a)
  descendM' f (K1 a) = liftM K1 (f a)
  transform' f (K1 a) = K1 (transform f a)
  transformM' f (K1 a) = liftM K1 (transformM f a)

instance Uniplate' (K1 i a) b where
  children' (K1 _) = []
  descend' _ (K1 a) = K1 a
  descendM' _ (K1 a) = return (K1 a)
  transform' _ (K1 a) = K1 a
  transformM' _ (K1 a) = return (K1 a)

instance (Uniplate' f b) => Uniplate' (M1 i c f) b where
  children' (M1 a) = children' a
  descend' f (M1 a) = M1 (descend' f a)
  descendM' f (M1 a) = liftM M1 (descendM' f a)
  transform' f (M1 a) = M1 (transform' f a)
  transformM' f (M1 a) = liftM M1 (transformM' f a)

instance (Uniplate' f b, Uniplate' g b) => Uniplate' (f :+: g) b where
  children' (L1 a) = children' a
  children' (R1 a) = children' a
  descend' f (L1 a) = L1 (descend' f a)
  descend' f (R1 a) = R1 (descend' f a)
  descendM' f (L1 a) = liftM L1 (descendM' f a)
  descendM' f (R1 a) = liftM R1 (descendM' f a)
  transform' f (L1 a) = L1 (transform' f a)
  transform' f (R1 a) = R1 (transform' f a)
  transformM' f (L1 a) = liftM L1 (transformM' f a)
  transformM' f (R1 a) = liftM R1 (transformM' f a)

instance (Uniplate' f b, Uniplate' g b) => Uniplate' (f :*: g) b where
  children' (a :*: b) = children' a ++ children' b 
  descend' f (a :*: b) = descend' f a :*: descend' f b 
  descendM' f (a :*: b) = liftM2 (:*:) (descendM' f a) (descendM' f b)
  transform' f (a :*: b) = transform' f a :*: transform' f b
  transformM' f (a :*: b) = liftM2 (:*:) (transformM' f a) (transformM' f b)


class Uniplate a where 
  children :: a -> [a]
#if __GLASGOW_HASKELL__ >= 701
  default children :: (Generic a, Uniplate' (Rep a) a) => a -> [a]
  children = childrendefault
#endif

  descend :: (a -> a) -> a -> a
#if __GLASGOW_HASKELL__ >= 701
  default descend :: (Generic a, Uniplate' (Rep a) a) => (a -> a) -> a -> a
  descend = descenddefault
#endif

  descendM :: Monad m => (a -> m a) -> a -> m a
#if __GLASGOW_HASKELL__ >= 701
  default descendM :: (Generic a, Uniplate' (Rep a) a, Monad m) => (a -> m a) -> a -> m a
  descendM = descendMdefault
#endif

  transform :: (a -> a) -> a -> a
#if __GLASGOW_HASKELL__ >= 701
  default transform :: (Generic a, Uniplate' (Rep a) a) => (a -> a) -> a -> a
  transform = transformdefault
#endif

  transformM :: Monad m => (a -> m a) -> a -> m a
#if __GLASGOW_HASKELL__ >= 701
  default transformM :: (Generic a, Uniplate' (Rep a) a, Monad m) => (a -> m a) -> a -> m a
  transformM = transformMdefault
#endif

childrendefault :: (Generic a, Uniplate' (Rep a) a) => a -> [a]
childrendefault = children' . from

descenddefault :: (Generic a, Uniplate' (Rep a) a) => (a -> a) -> a -> a
descenddefault f = to . descend' f . from

descendMdefault :: (Generic a, Uniplate' (Rep a) a, Monad m) => (a -> m a) -> a -> m a
descendMdefault f = liftM to . descendM' f . from

transformdefault :: (Generic a, Uniplate' (Rep a) a) => (a -> a) -> a -> a
transformdefault f = f . to . transform' f . from

transformMdefault :: (Generic a, Uniplate' (Rep a) a, Monad m) => (a -> m a) -> a -> m a
transformMdefault f = liftM to . transformM' f . from


-- Derived functions

universe :: Uniplate a => a -> [a]
universe a = build (go a)
  where
    go x cons nil = cons x $ foldr ($) nil $ map (\c -> go c cons) $ children x

rewrite :: Uniplate a => (a -> Maybe a) -> a -> a
rewrite f = transform g
  where
    g x = maybe x (rewrite f) (f x)

rewriteM :: (Monad m, Uniplate a) => (a -> m (Maybe a)) -> a -> m a
rewriteM f = transformM g
  where
    g x = f x >>= maybe (return x) (rewriteM f)

para :: Uniplate a => (a -> [r] -> r) -> a -> r
para f x = f x $ map (para f) $ children x


-- Base types instances
instance Uniplate Bool where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate Char where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate Double where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate Float where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate Int where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate () where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return

-- Tuple instances
instance Uniplate (b,c) where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (b,c,d) where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (b,c,d,e) where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (b,c,d,e,f) where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (b,c,d,e,f,g) where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (b,c,d,e,f,g,h) where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return

-- Parameterized type instances
instance Uniplate (Maybe a) where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (Either a b) where
  children _ = []
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return

instance Uniplate [a] where
  children []    = []
  children (_:t) = [t]
  descend _ []    = []
  descend f (h:t) = h:f t
  descendM _ []    = return []
  descendM f (h:t) = f t >>= \t' -> return (h:t')
  transform f []    = f []
  transform f (h:t) = f (h:transform f t)
  transformM f []    = f []
  transformM f (h:t) = transformM f t >>= \t' -> f (h:t')

