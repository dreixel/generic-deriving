{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
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
#if __GLASGOW_HASKELL__ >= 708
import Data.Proxy
#endif

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

#if __GLASGOW_HASKELL__ >= 708
instance (HasA a ~ flag, GComonad'' flag (a :*: w)) => GComonad' (a :*: w) where
  gduplicate' = gduplicate'' (Proxy :: Proxy flag)
  gextract' = gextract'' (Proxy :: Proxy flag)
#else
instance (GConstant' a, GFunctor' w, GComonad' w) => GComonad' (a :*: w) where
  gduplicate' w@(a :*: b) = gcast' a :*: gmap' (const w) b
  gextract' (_ :*: b) = gextract' b
  {-# INLINE gextract' #-}
#endif

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

With GHC 7.8 and above, instances are available for types like 
@data W a = W [t0 t1..tn] a [u0 u1..un]@
so long as the type derives 'Generic1'. (See below regarding GHC < 7.8.) Sum 
types where each constructor satisfies that condition are also accepted. Using
the type @a@ more than once is prohibited and will produce an error involving
the non-exported class @GConstant'@.

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

To make your type an instance of @Control.Comonad.Comonad@ from the @comonad@ package,
simply use 'gduplicate' and 'gextract':

@
instance Comonad W where
  duplicate = gduplicate
  extract = gextract
@

On versions of GHC before 7.8, one additional limitation is imposed: @a@ must
be the final field of each constructor.

@
-- GHC < 7.8:
data W a = W a Int -- Rejected
data W a = W Int a -- Accepted
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

instance (GConstant' f, GConstant' g) => GConstant' (f :*: g) where
  gcast' (a :*: b) = gcast' a :*: gcast' b

#if __GLASGOW_HASKELL__ >= 708
-- AdvancedOverlap apparatus to allow the :*: instance to choose which side to go down.
-- This could be backported to earlier versions of GHC.
type family HasA a :: Bool where
  HasA Par1       = True
  HasA (Rec1 a)   = True
  HasA (M1 x y z) = HasA z
  HasA a          = False

class GComonad'' (flag :: Bool) w where
  gduplicate'' :: proxy flag -> w a -> w (w a)
  gextract'' :: proxy flag -> w a -> a
  
instance (GConstant' a, GFunctor' w, GComonad' w) => GComonad'' False (a :*: w) where
  gduplicate'' _ w@(a :*: b) = gcast' a :*: gmap' (const w) b
  gextract'' _ (_ :*: b) = gextract' b
  {-# INLINE gextract'' #-}
  
instance (GConstant' w, GFunctor' a, GComonad' a) => GComonad'' True (a :*: w) where
  gduplicate'' _ w@(a :*: b) = gmap' (const w) a :*: gcast' b
  gextract'' _ (a :*: _) = gextract' a
  {-# INLINE gextract'' #-}
#endif
