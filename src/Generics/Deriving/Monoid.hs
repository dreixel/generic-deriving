{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

-- | This module provides two main features:
--
--     1. 'GMonoid', a generic version of the 'Monoid' type class, including instances
--     of the types from "Data.Monoid"
--
--     2. Default generic definitions for the 'Monoid' methods 'mempty' and 'mappend'
--
-- The generic defaults only work for types without alternatives (i.e. they have
-- only one constructor). We cannot in general know how to deal with different
-- constructors.

module Generics.Deriving.Monoid (

  -- * GMonoid type class
  GMonoid(..),

  -- * Default definitions
  -- ** GMonoid
  gmemptydefault,
  gmappenddefault,

  -- ** Monoid
  -- | These functions can be used in a 'Monoid' instance. For example:
  --
  -- @
  -- -- LANGUAGE DeriveGeneric
  --
  -- import Generics.Deriving.Base (Generic)
  -- import Generics.Deriving.Monoid
  --
  -- data T a = C a (Maybe a) deriving Generic
  --
  -- instance Monoid a => Monoid (T a) where
  --   mempty  = memptydefault
  --   mappend = mappenddefault
  -- @
  memptydefault,
  mappenddefault,

  -- * The Monoid module
  -- | This is exported for convenient access to the various wrapper types.
  module Data.Monoid,

  ) where

--------------------------------------------------------------------------------

import Generics.Deriving.Base
import Generics.Deriving.Instances ()
import Data.Monoid

--------------------------------------------------------------------------------

class GMonoid' f where
  gmempty'  :: f x
  gmappend' :: f x -> f x -> f x

instance GMonoid' U1 where
  gmempty' = U1
  gmappend' U1 U1 = U1

instance GMonoid a => GMonoid' (K1 i a) where
  gmempty' = K1 gmempty
  gmappend' (K1 x) (K1 y) = K1 (x `gmappend` y)

instance GMonoid' f => GMonoid' (M1 i c f) where
  gmempty' = M1 gmempty'
  gmappend' (M1 x) (M1 y) = M1 (x `gmappend'` y)

instance (GMonoid' f, GMonoid' h) => GMonoid' (f :*: h) where
  gmempty' = gmempty' :*: gmempty'
  gmappend' (x1 :*: y1) (x2 :*: y2) = gmappend' x1 x2 :*: gmappend' y1 y2

--------------------------------------------------------------------------------

gmemptydefault :: (Generic a, GMonoid' (Rep a)) => a
gmemptydefault = to gmempty'

gmappenddefault :: (Generic a, GMonoid' (Rep a)) => a -> a -> a
gmappenddefault x y = to (gmappend' (from x) (from y))

--------------------------------------------------------------------------------

class Monoid' f where
  mempty'  :: f x
  mappend' :: f x -> f x -> f x

instance Monoid' U1 where
  mempty' = U1
  mappend' U1 U1 = U1

instance Monoid a => Monoid' (K1 i a) where
  mempty' = K1 mempty
  mappend' (K1 x) (K1 y) = K1 (x `mappend` y)

instance Monoid' f => Monoid' (M1 i c f) where
  mempty' = M1 mempty'
  mappend' (M1 x) (M1 y) = M1 (x `mappend'` y)

instance (Monoid' f, Monoid' h) => Monoid' (f :*: h) where
  mempty' = mempty' :*: mempty'
  mappend' (x1 :*: y1) (x2 :*: y2) = mappend' x1 x2 :*: mappend' y1 y2

--------------------------------------------------------------------------------

memptydefault :: (Generic a, Monoid' (Rep a)) => a
memptydefault = to mempty'

mappenddefault :: (Generic a, Monoid' (Rep a)) => a -> a -> a
mappenddefault x y = to (mappend' (from x) (from y))

--------------------------------------------------------------------------------

class GMonoid a where

  -- | Generic 'mempty'
  gmempty  :: a

  -- | Generic 'mappend'
  gmappend :: a -> a -> a

  -- | Generic 'mconcat'
  gmconcat :: [a] -> a
  gmconcat = foldr gmappend gmempty

#if __GLASGOW_HASKELL__ >= 701
  default gmempty :: (Generic a, GMonoid' (Rep a)) => a
  gmempty = to gmempty'

  default gmappend :: (Generic a, GMonoid' (Rep a)) => a -> a -> a
  gmappend x y = to (gmappend' (from x) (from y))
#endif

--------------------------------------------------------------------------------

-- Instances that reuse Monoid
instance GMonoid Ordering where
  gmempty = mempty
  gmappend = mappend
instance GMonoid () where
  gmempty = mempty
  gmappend = mappend
instance GMonoid Any where
  gmempty = mempty
  gmappend = mappend
instance GMonoid All where
  gmempty = mempty
  gmappend = mappend
instance GMonoid (First a) where
  gmempty = mempty
  gmappend = mappend
instance GMonoid (Last a) where
  gmempty = mempty
  gmappend = mappend
instance Num a => GMonoid (Sum a) where
  gmempty = mempty
  gmappend = mappend
instance Num a => GMonoid (Product a) where
  gmempty = mempty
  gmappend = mappend
instance GMonoid [a] where
  gmempty  = mempty
  gmappend = mappend
instance GMonoid (Endo a) where
  gmempty = mempty
  gmappend = mappend

-- Handwritten instances
instance GMonoid a => GMonoid (Dual a) where
  gmempty = Dual gmempty
  gmappend (Dual x) (Dual y) = Dual (gmappend y x)
instance GMonoid a => GMonoid (Maybe a) where
  gmempty = Nothing
  gmappend Nothing  x        = x
  gmappend x        Nothing  = x
  gmappend (Just x) (Just y) = Just (gmappend x y)
instance GMonoid b => GMonoid (a -> b) where
  gmempty _ = gmempty
  gmappend f g x = gmappend (f x) (g x)

-- Tuple instances
instance (GMonoid a,GMonoid b) => GMonoid (a,b) where
  gmempty = (gmempty,gmempty)
  gmappend (a1,b1) (a2,b2) =
    (gmappend a1 a2,gmappend b1 b2)
instance (GMonoid a,GMonoid b,GMonoid c) => GMonoid (a,b,c) where
  gmempty = (gmempty,gmempty,gmempty)
  gmappend (a1,b1,c1) (a2,b2,c2) =
    (gmappend a1 a2,gmappend b1 b2,gmappend c1 c2)
instance (GMonoid a,GMonoid b,GMonoid c,GMonoid d) => GMonoid (a,b,c,d) where
  gmempty = (gmempty,gmempty,gmempty,gmempty)
  gmappend (a1,b1,c1,d1) (a2,b2,c2,d2) =
    (gmappend a1 a2,gmappend b1 b2,gmappend c1 c2,gmappend d1 d2)
instance (GMonoid a,GMonoid b,GMonoid c,GMonoid d,GMonoid e) => GMonoid (a,b,c,d,e) where
  gmempty = (gmempty,gmempty,gmempty,gmempty,gmempty)
  gmappend (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) =
    (gmappend a1 a2,gmappend b1 b2,gmappend c1 c2,gmappend d1 d2,gmappend e1 e2)
instance (GMonoid a,GMonoid b,GMonoid c,GMonoid d,GMonoid e,GMonoid f) => GMonoid (a,b,c,d,e,f) where
  gmempty = (gmempty,gmempty,gmempty,gmempty,gmempty,gmempty)
  gmappend (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) =
    (gmappend a1 a2,gmappend b1 b2,gmappend c1 c2,gmappend d1 d2,gmappend e1 e2,gmappend f1 f2)
instance (GMonoid a,GMonoid b,GMonoid c,GMonoid d,GMonoid e,GMonoid f,GMonoid g) => GMonoid (a,b,c,d,e,f,g) where
  gmempty = (gmempty,gmempty,gmempty,gmempty,gmempty,gmempty,gmempty)
  gmappend (a1,b1,c1,d1,e1,f1,g1) (a2,b2,c2,d2,e2,f2,g2) =
    (gmappend a1 a2,gmappend b1 b2,gmappend c1 c2,gmappend d1 d2,gmappend e1 e2,gmappend f1 f2,gmappend g1 g2)
instance (GMonoid a,GMonoid b,GMonoid c,GMonoid d,GMonoid e,GMonoid f,GMonoid g,GMonoid h) => GMonoid (a,b,c,d,e,f,g,h) where
  gmempty = (gmempty,gmempty,gmempty,gmempty,gmempty,gmempty,gmempty,gmempty)
  gmappend (a1,b1,c1,d1,e1,f1,g1,h1) (a2,b2,c2,d2,e2,f2,g2,h2) =
    (gmappend a1 a2,gmappend b1 b2,gmappend c1 c2,gmappend d1 d2,gmappend e1 e2,gmappend f1 f2,gmappend g1 g2,gmappend h1 h2)

