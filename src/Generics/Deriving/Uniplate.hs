{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 705
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE OverlappingInstances #-}
#endif

{- |
Module      :  Generics.Deriving.Uniplate
Copyright   :  2011-2012 Universiteit Utrecht, University of Oxford
License     :  BSD3

Maintainer  :  generics@haskell.org
Stability   :  experimental
Portability :  non-portable

Summary: Functions inspired by the Uniplate generic programming library,
mostly implemented by Sean Leather.
-}

module Generics.Deriving.Uniplate (
  -- * Generic Uniplate class
    Uniplate(..)

  -- * Derived functions
  , uniplate
  , universe
  , rewrite
  , rewriteM
  , contexts
  , holes
  , para

  -- * Default definitions
  , childrendefault
  , contextdefault
  , descenddefault
  , descendMdefault
  , transformdefault
  , transformMdefault

  -- * Internal Uniplate class
  , Uniplate'(..)

  -- * Internal Context class
  , Context'(..)
  ) where


import Generics.Deriving.Base

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

instance
#if __GLASGOW_HASKELL__ >= 709
    {-# OVERLAPPING #-}
#endif
    (Uniplate a) => Uniplate' (K1 i a) a where
  children' (K1 a) = [a]
  descend' f (K1 a) = K1 (f a)
  descendM' f (K1 a) = liftM K1 (f a)
  transform' f (K1 a) = K1 (transform f a)
  transformM' f (K1 a) = liftM K1 (transformM f a)

instance
#if __GLASGOW_HASKELL__ >= 709
    {-# OVERLAPPABLE #-}
#endif
    Uniplate' (K1 i a) b where
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


-- Context' is a separate class from Uniplate' since it uses special product
-- instances, but the context function still appears in Uniplate.
class Context' f b where
  context' :: f a -> [b] -> f a

instance Context' U1 b where
  context' U1 _ = U1

instance
#if __GLASGOW_HASKELL__ >= 709
    {-# OVERLAPPING #-}
#endif
    Context' (K1 i a) a where
  context' _      []    = error "Generics.Deriving.Uniplate.context: empty list"
  context' (K1 _) (c:_) = K1 c

instance
#if __GLASGOW_HASKELL__ >= 709
    {-# OVERLAPPABLE #-}
#endif
    Context' (K1 i a) b where
  context' (K1 a) _ = K1 a

instance (Context' f b) => Context' (M1 i c f) b where
  context' (M1 a) cs = M1 (context' a cs)

instance (Context' f b, Context' g b) => Context' (f :+: g) b where
  context' (L1 a) cs = L1 (context' a cs)
  context' (R1 a) cs = R1 (context' a cs)

instance
#if __GLASGOW_HASKELL__ >= 709
    {-# OVERLAPPING #-}
#endif
    (Context' g a) => Context' (M1 i c (K1 j a) :*: g) a where
  context' _                 []     = error "Generics.Deriving.Uniplate.context: empty list"
  context' (M1 (K1 _) :*: b) (c:cs) = M1 (K1 c) :*: context' b cs

instance
#if __GLASGOW_HASKELL__ >= 709
    {-# OVERLAPPABLE #-}
#endif
    (Context' g b) => Context' (f :*: g) b where
  context' (a :*: b) cs = a :*: context' b cs


class Uniplate a where
  children :: a -> [a]
#if __GLASGOW_HASKELL__ >= 701
  default children :: (Generic a, Uniplate' (Rep a) a) => a -> [a]
  children = childrendefault
#endif

  context :: a -> [a] -> a
#if __GLASGOW_HASKELL__ >= 701
  default context :: (Generic a, Context' (Rep a) a) => a -> [a] -> a
  context = contextdefault
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

contextdefault :: (Generic a, Context' (Rep a) a) => a -> [a] -> a
contextdefault x cs = to (context' (from x) cs)

descenddefault :: (Generic a, Uniplate' (Rep a) a) => (a -> a) -> a -> a
descenddefault f = to . descend' f . from

descendMdefault :: (Generic a, Uniplate' (Rep a) a, Monad m) => (a -> m a) -> a -> m a
descendMdefault f = liftM to . descendM' f . from

transformdefault :: (Generic a, Uniplate' (Rep a) a) => (a -> a) -> a -> a
transformdefault f = f . to . transform' f . from

transformMdefault :: (Generic a, Uniplate' (Rep a) a, Monad m) => (a -> m a) -> a -> m a
transformMdefault f = liftM to . transformM' f . from


-- Derived functions (mostly copied from Neil Michell's code)

uniplate :: Uniplate a => a -> ([a], [a] -> a)
uniplate a = (children a, context a)

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

contexts :: Uniplate a => a -> [(a, a -> a)]
contexts a = (a, id) : f (holes a)
  where
    f xs = [ (ch2, ctx1 . ctx2)
           | (ch1, ctx1) <- xs
           , (ch2, ctx2) <- contexts ch1]

holes :: Uniplate a => a -> [(a, a -> a)]
holes a = uncurry f (uniplate a)
  where
    f []     _   = []
    f (x:xs) gen = (x, gen . (:xs)) : f xs (gen . (x:))

para :: Uniplate a => (a -> [r] -> r) -> a -> r
para f x = f x $ map (para f) $ children x


-- Base types instances
instance Uniplate Bool where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate Char where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate Double where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate Float where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate Int where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate () where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return

-- Tuple instances
instance Uniplate (b,c) where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (b,c,d) where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (b,c,d,e) where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (b,c,d,e,f) where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (b,c,d,e,f,g) where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (b,c,d,e,f,g,h) where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return

-- Parameterized type instances
instance Uniplate (Maybe a) where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return
instance Uniplate (Either a b) where
  children _ = []
  context x _ = x
  descend _ = id
  descendM _ = return
  transform = id
  transformM _ = return

instance Uniplate [a] where
  children []    = []
  children (_:t) = [t]
  context _     []    = error "Generics.Deriving.Uniplate.context: empty list"
  context []    _     = []
  context (h:_) (t:_) = h:t
  descend _ []    = []
  descend f (h:t) = h:f t
  descendM _ []    = return []
  descendM f (h:t) = f t >>= \t' -> return (h:t')
  transform f []    = f []
  transform f (h:t) = f (h:transform f t)
  transformM f []    = f []
  transformM f (h:t) = transformM f t >>= \t' -> f (h:t')

