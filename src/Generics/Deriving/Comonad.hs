{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

module Generics.Deriving.Comonad (
  -- * GComonad class
  GComonad(..)
  
  -- * Default methods
  , gduplicatedefault
  , gextractdefault
  
  ) where

import Generics.Deriving.Base
import Generics.Deriving.Instances ()
import Generics.Deriving.Internal.Functor

import Data.Monoid (Monoid, mappend, mempty )


class GComonad' w where
  gduplicate' :: w a -> w (w a)
  gextract' :: w a -> a

instance GComonad' Par1 where
  gduplicate' = Par1
  {-# INLINE gduplicate' #-}
  gextract'   = unPar1
  {-# INLINE gextract' #-}

instance (GComonad f, GFunctor f) => GComonad' (Rec1 f) where
  gduplicate' w@(Rec1 a) = Rec1 $ gmap (const w) a
  gextract' (Rec1 a) = gextract a
  {-# INLINE gextract' #-}

instance (GConstant' a, GFunctor' w, GComonad' w) => GComonad' (a :*: w) where
  gduplicate' w@(a :*: b) = gcast' a :*: gmap' (const w) b
  gextract' (_ :*: b) = gextract' b
  {-# INLINE gextract' #-}

instance (GFunctor' f, GFunctor' g, GComonad' f, GComonad' g) => GComonad' (f :+: g) where
  gduplicate' w@(L1 a) = L1 . gmap' (const w) $ a
  gduplicate' w@(R1 a) = R1 . gmap' (const w) $ a
  gextract' (L1 a) = gextract' a
  gextract' (R1 a) = gextract' a
  {-# INLINE gextract' #-}
  
instance (GComonad' f, GFunctor' f) => GComonad' (M1 i c f) where
  gduplicate' w@(M1 a) = M1 (gmap' (const w) a)
  gextract' (M1 a) = gextract' a
  {-# INLINE gextract' #-}

instance (GFunctor f, GFunctor' g, GComonad f, GComonad' g) => GComonad' (f :.: g) where
  gduplicate' w@(Comp1 x) = gmap' (const $ Comp1 x) w
  gextract' (Comp1 x) = gextract' $ gextract x
  {-# INLINE gextract' #-}

{- |

Generically derived comonads.

Instances are available for any type of the form 

@
data W a =
    C1 [t11 t12 .. t1N] a
 [| C2 [t21 t22 .. t2N] a
  | .. ]
  deriving ('Generic1')
instance 'GFunctor' W 
instance 'GComonad' W
@

In other words, each constructor must include @a@ in the rightmost position.
It may appear in no other position. Attempting to derive 'GComonad' for a datatype
like @data Pair a = Pair a a deriving Generic1@ will result in an error
involving the unexported class @GConstant'@.

You may also wrap a type that is itself an instance of 'GComonad':

@
data W' a = C'1 a | C'2 (W a) deriving ('Generic1')
instance 'GComonad' W'
@

And you can use @'Monoid' m => 'GComonad' ((->) m)@:

@
data W'' a = C'' ((Product Int) -> a) deriving ('Generic1')
instance 'GComonad' W''
@
-}
class GComonad w where
  gduplicate :: w a -> w (w a)
#if __GLASGOW_HASKELL__ >= 701
  default gduplicate :: (Generic1 w, GFunctor' (Rep1 w), GComonad' (Rep1 w))
                     => w a -> w (w a)
  gduplicate = to1 . gmap' to1 . gduplicate' . from1
#endif
  
  gextract :: w a -> a
#if __GLASGOW_HASKELL__ >= 701
  default gextract :: (Generic1 w, GComonad' (Rep1 w))
                   => w a -> a
  gextract = gextract' . from1
#endif

gduplicatedefault :: (Generic1 w, GFunctor' (Rep1 w), GComonad' (Rep1 w))
                  => w a -> w (w a)
gduplicatedefault = to1 . gmap' to1 . gduplicate' . from1

gextractdefault :: (Generic1 w, GComonad' (Rep1 w))
                => w a -> a
gextractdefault = gextract' . from1

-- Instances for Prelude types, to enable use in derived types

instance Monoid m => GComonad ((->) m) where
  gduplicate f m = f . mappend m
  {-# INLINE gduplicate #-}
  gextract f = f mempty
  {-# INLINE gextract #-}

-- used in (:*:) instances to describe the "side" which does not use a
-- Expresses the constraint that (w a) does not involve a.
class GConstant' w where
  gcast' :: w a -> w b

instance GConstant' (K1 i c) where
  gcast' (K1 a) = K1 a

instance GConstant' U1 where
  gcast' U1 = U1
  
instance (GConstant' f) => GConstant' (M1 i c f) where
  gcast' (M1 a) = M1 (gcast' a)
