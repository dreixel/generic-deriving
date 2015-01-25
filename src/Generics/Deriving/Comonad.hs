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

instance (GFunctor' a, GFunctor' w, GComonad' w) => GComonad' (a :*: w) where
  gduplicate' w@(a :*: b) = gmap' (const w) a :*: gmap' (const w) b
  gextract' (_ :*: b) = gextract' b

instance (GFunctor' f, GFunctor' g, GComonad' f, GComonad' g) => GComonad' (f :+: g) where
  gduplicate' w@(L1 a) = L1 . gmap' (const w) $ a
  gduplicate' w@(R1 a) = R1 . gmap' (const w) $ a
  gextract' (L1 a) = gextract' a
  gextract' (R1 a) = gextract' a
  
instance (GComonad' f, GFunctor' f) => GComonad' (M1 i c f) where
  gduplicate' w@(M1 a) = M1 (gmap' (const w) a)
  gextract' (M1 a) = gextract' a

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
@

In other words, each constructor must include @a@ in the rightmost position.

If @a@ appears in other positions, it will be ignored by 'gextract'.
The resulting `GComonad` instance will therefore violate the comonad laws.

-}
class GComonad w where
  gduplicate :: w a -> w (w a)
  default gduplicate :: (Generic1 w, GFunctor' (Rep1 w), GComonad' (Rep1 w))
                     => w a -> w (w a)
  gduplicate = to1 . gmap' to1 . gduplicate' . from1
  
  gextract :: w a -> a
  default gextract :: (Generic1 w, GComonad' (Rep1 w))
                   => w a -> a
  gextract = gextract' . from1

gduplicatedefault :: (Generic1 w, GFunctor' (Rep1 w), GComonad' (Rep1 w))
                  => w a -> w (w a)
gduplicatedefault = to1 . gmap' to1 . gduplicate' . from1

gextractdefault :: (Generic1 w, GComonad' (Rep1 w))
                => w a -> a
gextractdefault = gextract' . from1

