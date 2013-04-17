{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

module Generics.Deriving.Comonad (
  -- * GComonad class
    GComonad(..), 
    GComo(..) 

  -- * Default method
  , gextenddefault
  , gextractdefault
  , gextedefault

  ) where

import Generics.Deriving.Base
import Generics.Deriving.Instances ()

--------------------------------------------------------------------------------
-- Generic extend/extract
--------------------------------------------------------------------------------

class Parametric (t :: * -> *)
instance Parametric Par1
instance Parametric f => Parametric (Rec1 f)
instance Parametric f => Parametric (M1 i c f)
instance (Parametric f) => Parametric (f :*: g)
-- instance (Parametric g) => Parametric (f :*: g) hmm
instance (Parametric f, Parametric g) => Parametric (f :+: g)


class GComo' d r where
  gexte' :: (d a -> b) -> r a -> r b

instance GComo' d U1 where
  gexte' _ U1 = U1

instance (Generic1 d, Rep1 d ~ Par1) => GComo' d Par1 where
  gexte' f (Par1 x) = Par1 (f (to1 $ Par1 x))

instance (GComo' d f) => GComo' d (M1 i c f) where
  gexte' f (M1 a) = M1 (gexte' f a)

instance (GComo' d f, GComo' d g) => GComo' d (f :+: g) where
  gexte' f (L1 a) = L1 (gexte' f a)
  gexte' f (R1 a) = R1 (gexte' f a)

instance (GComo' d f, GComo' d g) => GComo' d (f :*: g) where
  gexte' f (a :*: b) = (gexte' f a) :*: (gexte' f b)

instance (Generic1 d, GComo' (Rep1 d) d) => GComo' d (Rec1 d) where
  gexte' f (Rec1 x) = Rec1 (gexte' (f . to1) x)

--instance GComo d Par1 where
--  gexte f (Par1 a) = Par1 (f (Par1 a))

--class GCojoin d where
--    gcojoin :: d a -> d (d a)

--instance GCojoin U1 where
--    gcojoin U1 = U1

--instance GCojoin Par1 where
--    gcojoin (Par1 a) = Par1 (Par1 a)

--instance GCojoin (K1 R c) where
--    gcojoin (K1 a) = K1 (gcojoin a)

--instance GCojoin (K1 P c) where
--    gcojoin (K1 a) = K1 (K1 a)




class GComonad' d where
  gextract' :: d a -> a
  gextend' :: (d a -> b) -> d a -> d b

instance GComonad' U1 where
  gextract' U1 = error "extract undefined for nullary constructors"
  gextend' _ U1 = U1

instance GComonad' Par1 where
  gextract' (Par1 a) = a
  gextend' f (Par1 a) = Par1 (f (Par1 a))

instance GComonad' (K1 i c) where
  gextract' (K1 a) = error "extract undefined for non-parameteric constructors"
  gextend' f (K1 a) = K1 a

instance (GComonad f) => GComonad' (Rec1 f) where
  gextract' (Rec1 a) = gextract a
  gextend' f (Rec1 a) = Rec1 (gextend (f . Rec1) a)

instance (GComonad' f) => GComonad' (M1 i c f) where
  gextract' (M1 a) = gextract' a
  gextend' f (M1 a) = M1 (gextend' (f . M1) a)

instance (GComonad' f, GComonad' g) => GComonad' (f :+: g) where
  gextract' (L1 a) = gextract' a
  gextract' (R1 a) = gextract' a
  gextend' f (L1 a) = L1 (gextend' (f . L1) a)
  gextend' f (R1 a) = R1 (gextend' (f . R1) a)

instance (GComonad' f, GComonad' g) => GComonad' (f :*: g) where
  gextract' (a :*: b) = gextract' a -- left bias
  gextend' f (a :*: b) = (gextend' (\a' -> f (a' :*: b)) a) :*: (gextend' (\b' -> f (a :*: b')) b)

--  gextend' f (a :*: b) = (gextend' (\a' -> f (a' :*: b)) a) :*: (gextend' (\b' -> f (a :*: b')) b)

-- (gextend f a) :*: (gextend f b)

--instance (GComonad f, GComonad' g) => GComonad' (f :.: g) where
--  gextract' (Comp1 x) = gextract . gextract' $ x
--  gextend' f (Comp1 x) = Comp1 (gextend (gextend' f) x)

class GComo d where
  gexte :: (d a -> b) -> d a -> d b
#if __GLASGOW_HASKELL__ >= 701
  default gexte :: (Generic1 d, GComo' d (Rep1 d))
               => (d a -> b) -> d a -> d b
  gexte = gextedefault
#endif

gextedefault :: (Generic1 d, GComo' d (Rep1 d))
            => (d a -> b) -> d a -> d b
gextedefault f = to1 . (gexte' f) . from1


class GComonad d where
  gextract :: d a -> a
  gextend :: (d a -> b) -> d a -> d b
#if __GLASGOW_HASKELL__ >= 701
  default gextend :: (Generic1 d, GComonad' (Rep1 d))
               => (d a -> b) -> d a -> d b
  gextend = gextenddefault
  default gextract :: (Generic1 d, GComonad' (Rep1 d))
               => d a -> a
  gextract = gextractdefault
#endif

gextenddefault :: (Generic1 d, GComonad' (Rep1 d))
            => (d a -> b) -> d a -> d b
gextenddefault f = to1 . gextend' (f . to1) . from1

gextractdefault :: (Generic1 d, GComonad' (Rep1 d))
            => d a -> a
gextractdefault = gextract' . from1

-- Base types instances
instance GComonad [] where
  gextract = gextractdefault
  gextend = gextenddefault
