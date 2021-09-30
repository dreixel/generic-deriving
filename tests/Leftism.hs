{-# language CPP #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 708
{-# language EmptyCase #-}
#endif

-- | An alternative version of GFunctor for 'ComposeLeft'-derived
-- instances.
module Leftism where
import Generics.Deriving.TH
import GHC.Exts (Int#)
import GHC.Generics

class GFunctor' f where
  gmap' :: (a -> b) -> f a -> f b

instance GFunctor' V1 where
  gmap' _ x = case x of
#if __GLASGOW_HASKELL__ >= 708
                {}
#else
                !_ -> error "Void gmap"
#endif

instance GFunctor' U1 where
  gmap' _ U1 = U1

instance GFunctor' Par1 where
  gmap' f (Par1 a) = Par1 (f a)

instance GFunctor' (K1 i c) where
  gmap' _ (K1 a) = K1 a

instance (GFunctor f) => GFunctor' (Rec1 f) where
  gmap' f (Rec1 a) = Rec1 (gmap f a)

instance (GFunctor' f) => GFunctor' (M1 i c f) where
  gmap' f (M1 a) = M1 (gmap' f a)

instance (GFunctor' f, GFunctor' g) => GFunctor' (f :+: g) where
  gmap' f (L1 a) = L1 (gmap' f a)
  gmap' f (R1 a) = R1 (gmap' f a)

instance (GFunctor' f, GFunctor' g) => GFunctor' (f :*: g) where
  gmap' f (a :*: b) = gmap' f a :*: gmap' f b

instance (GFunctor' f, GFunctor g) => GFunctor' (f :.: g) where
  gmap' f (Comp1 x) = Comp1 (gmap' (gmap f) x)

instance GFunctor' UAddr where
  gmap' _ (UAddr a) = UAddr a

instance GFunctor' UChar where
  gmap' _ (UChar c) = UChar c

instance GFunctor' UDouble where
  gmap' _ (UDouble d) = UDouble d

instance GFunctor' UFloat where
  gmap' _ (UFloat f) = UFloat f

instance GFunctor' UInt where
  gmap' _ (UInt i) = UInt i

instance GFunctor' UWord where
  gmap' _ (UWord w) = UWord w

class GFunctor f where
  gmap :: (a -> b) -> f a -> f b

instance GFunctor (Either Int) where
  gmap = fmap
instance GFunctor Maybe where
  gmap = fmap

gmapdefault :: (Generic1 f, GFunctor' (Rep1 f))
            => (a -> b) -> f a -> f b
gmapdefault f = to1 . gmap' f . from1

newtype Lefty a = Lefty (Maybe (Either Int a))
$(deriveAll1Options defaultOptions{composeLeftOptions=ComposeLeft} ''Lefty)
$(deriveAll0 ''Lefty)

instance GFunctor Lefty where gmap = gmapdefault

type family Grum a :: * -> *
type instance Grum Int = Lefty

newtype Leftist a = Leftist (Maybe (Grum Int a))
$(deriveAll1Options defaultOptions{composeLeftOptions=ComposeLeft} ''Leftist)

instance GFunctor Leftist where gmap = gmapdefault

data Boring a = Boring
$(deriveAll1Options defaultOptions{composeLeftOptions=ComposeLeft} ''Boring)

instance GFunctor Boring where gmap = gmapdefault

newtype StillBoring a = StillBoring (Maybe (Grum Int Bool))
$(deriveAll1Options defaultOptions{composeLeftOptions=ComposeLeft} ''StillBoring)

instance GFunctor StillBoring where gmap = gmapdefault

newtype Iddish a = Iddish a
$(deriveAll1Options defaultOptions{composeLeftOptions=ComposeLeft} ''Iddish)

instance GFunctor Iddish where gmap = gmapdefault

data Potato a = Potato a Int#
$(deriveAll1Options defaultOptions{composeLeftOptions=ComposeLeft} ''Potato)
instance GFunctor Potato where gmap = gmapdefault

-- Woohoo!
newtype Fix f a = Fix (f (Fix f a))
$(deriveAll1Options defaultOptions{composeLeftOptions=ComposeLeft} ''Fix)

instance GFunctor f => GFunctor (Fix f) where gmap = gmapdefault

-- This also doesn't work with the standard deriveAll1.
newtype Foo f a = Foo (f (Maybe a))
$(deriveAll1Options defaultOptions{composeLeftOptions=ComposeLeft} ''Foo)

instance GFunctor f => GFunctor (Foo f) where gmap = gmapdefault

newtype Constant a b = Constant a
$(deriveAll1Options defaultOptions{composeLeftOptions=ComposeLeft} ''Constant)
