{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

#if __GLASGOW_HASKELL__ >= 705
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module Generics.Deriving.Semigroup.Internal (
  -- * Generic semigroup class
    GSemigroup(..)

  -- * Default definition
  , gsappenddefault

  -- * Internal semigroup class
  , GSemigroup'(..)

  ) where

import Control.Applicative
import Data.Monoid as Monoid
#if MIN_VERSION_base(4,5,0)
  hiding ((<>))
#endif
import Generics.Deriving.Base

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down)
#else
import GHC.Exts (Down)
#endif

#if MIN_VERSION_base(4,7,0)
import Data.Proxy (Proxy)
#endif

#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity (Identity)
import Data.Void (Void)
#endif

#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup as Semigroup
#endif

-------------------------------------------------------------------------------

infixr 6 `gsappend'`
class GSemigroup' f where
  gsappend' :: f x -> f x -> f x

instance GSemigroup' U1 where
  gsappend' U1 U1 = U1

instance GSemigroup a => GSemigroup' (K1 i a) where
  gsappend' (K1 x) (K1 y) = K1 (gsappend x y)

instance GSemigroup' f => GSemigroup' (M1 i c f) where
  gsappend' (M1 x) (M1 y) = M1 (gsappend' x y)

instance (GSemigroup' f, GSemigroup' g) => GSemigroup' (f :*: g) where
  gsappend' (x1 :*: y1) (x2 :*: y2) = gsappend' x1 x2 :*: gsappend' y1 y2

-------------------------------------------------------------------------------

infixr 6 `gsappend`
class GSemigroup a where
  gsappend :: a -> a -> a
#if __GLASGOW_HASKELL__ >= 701
  default gsappend :: (Generic a, GSemigroup' (Rep a)) => a -> a -> a
  gsappend = gsappenddefault
#endif

  gstimes :: Integral b => b -> a -> a
  gstimes y0 x0
    | y0 <= 0   = error "gstimes: positive multiplier expected"
    | otherwise = f x0 y0
    where
      f x y
        | even y = f (gsappend x x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (gsappend x x) (pred y  `quot` 2) x
      g x y z
        | even y = g (gsappend x x) (y `quot` 2) z
        | y == 1 = gsappend x z
        | otherwise = g (gsappend x x) (pred y `quot` 2) (gsappend x z)

#if MIN_VERSION_base(4,9,0)
  -- | Only available with @base-4.9@ or later
  gsconcat :: NonEmpty a -> a
  gsconcat (a :| as) = go a as where
    go b (c:cs) = gsappend b (go c cs)
    go b []     = b
#endif

infixr 6 `gsappenddefault`
gsappenddefault :: (Generic a, GSemigroup' (Rep a)) => a -> a -> a
gsappenddefault x y = to (gsappend' (from x) (from y))

-------------------------------------------------------------------------------

-- Instances that reuse Monoid
instance GSemigroup Ordering where
  gsappend = mappend
instance GSemigroup () where
  gsappend = mappend
instance GSemigroup Any where
  gsappend = mappend
instance GSemigroup All where
  gsappend = mappend
instance GSemigroup (Monoid.First a) where
  gsappend = mappend
instance GSemigroup (Monoid.Last a) where
  gsappend = mappend
instance Num a => GSemigroup (Sum a) where
  gsappend = mappend
instance Num a => GSemigroup (Product a) where
  gsappend = mappend
instance GSemigroup [a] where
  gsappend = mappend
instance GSemigroup (Endo a) where
  gsappend = mappend
#if MIN_VERSION_base(4,8,0)
instance Alternative f => GSemigroup (Alt f a) where
  gsappend = mappend
#endif

-- Handwritten instances
instance GSemigroup a => GSemigroup (Dual a) where
  gsappend (Dual x) (Dual y) = Dual (gsappend y x)
instance GSemigroup a => GSemigroup (Maybe a) where
  gsappend Nothing  x        = x
  gsappend x        Nothing  = x
  gsappend (Just x) (Just y) = Just (gsappend x y)
instance GSemigroup b => GSemigroup (a -> b) where
  gsappend f g x = gsappend (f x) (g x)
instance GSemigroup a => GSemigroup (Const a b) where
  gsappend = gsappenddefault
instance GSemigroup a => GSemigroup (Down a) where
  gsappend = gsappenddefault
instance GSemigroup (Either a b) where
  gsappend Left{} b = b
  gsappend a      _ = a

#if MIN_VERSION_base(4,7,0)
instance GSemigroup
# if MIN_VERSION_base(4,9,0)
                 (Proxy s)
# else
                 (Proxy (s :: *))
# endif
                 where
  gsappend    = gsappenddefault
#endif

#if MIN_VERSION_base(4,8,0)
instance GSemigroup a => GSemigroup (Identity a) where
  gsappend = gsappenddefault

instance GSemigroup Void where
  gsappend a _ = a
#endif

#if MIN_VERSION_base(4,9,0)
instance GSemigroup (Semigroup.First a) where
  gsappend = (<>)

instance GSemigroup (Semigroup.Last a) where
  gsappend = (<>)

instance Ord a => GSemigroup (Max a) where
  gsappend = (<>)

instance Ord a => GSemigroup (Min a) where
  gsappend = (<>)

instance GSemigroup (NonEmpty a) where
  gsappend = (<>)

instance GSemigroup a => GSemigroup (Option a) where
  gsappend (Option a) (Option b) = Option (gsappend a b)
#endif

-- Tuple instances
instance (GSemigroup a,GSemigroup b) => GSemigroup (a,b) where
  gsappend (a1,b1) (a2,b2) =
    (gsappend a1 a2,gsappend b1 b2)
instance (GSemigroup a,GSemigroup b,GSemigroup c) => GSemigroup (a,b,c) where
  gsappend (a1,b1,c1) (a2,b2,c2) =
    (gsappend a1 a2,gsappend b1 b2,gsappend c1 c2)
instance (GSemigroup a,GSemigroup b,GSemigroup c,GSemigroup d) => GSemigroup (a,b,c,d) where
  gsappend (a1,b1,c1,d1) (a2,b2,c2,d2) =
    (gsappend a1 a2,gsappend b1 b2,gsappend c1 c2,gsappend d1 d2)
instance (GSemigroup a,GSemigroup b,GSemigroup c,GSemigroup d,GSemigroup e) => GSemigroup (a,b,c,d,e) where
  gsappend (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) =
    (gsappend a1 a2,gsappend b1 b2,gsappend c1 c2,gsappend d1 d2,gsappend e1 e2)
instance (GSemigroup a,GSemigroup b,GSemigroup c,GSemigroup d,GSemigroup e,GSemigroup f) => GSemigroup (a,b,c,d,e,f) where
  gsappend (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) =
    (gsappend a1 a2,gsappend b1 b2,gsappend c1 c2,gsappend d1 d2,gsappend e1 e2,gsappend f1 f2)
instance (GSemigroup a,GSemigroup b,GSemigroup c,GSemigroup d,GSemigroup e,GSemigroup f,GSemigroup g) => GSemigroup (a,b,c,d,e,f,g) where
  gsappend (a1,b1,c1,d1,e1,f1,g1) (a2,b2,c2,d2,e2,f2,g2) =
    (gsappend a1 a2,gsappend b1 b2,gsappend c1 c2,gsappend d1 d2,gsappend e1 e2,gsappend f1 f2,gsappend g1 g2)
instance (GSemigroup a,GSemigroup b,GSemigroup c,GSemigroup d,GSemigroup e,GSemigroup f,GSemigroup g,GSemigroup h) => GSemigroup (a,b,c,d,e,f,g,h) where
  gsappend (a1,b1,c1,d1,e1,f1,g1,h1) (a2,b2,c2,d2,e2,f2,g2,h2) =
    (gsappend a1 a2,gsappend b1 b2,gsappend c1 c2,gsappend d1 d2,gsappend e1 e2,gsappend f1 f2,gsappend g1 g2,gsappend h1 h2)
