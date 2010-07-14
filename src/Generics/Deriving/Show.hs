{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE IncoherentInstances #-} -- :-/
{-# LANGUAGE CPP #-}

module Generics.Deriving.Show (
  -- * Generic show class
    GShow(..)

  -- * Default definition
  , gshowsdefault

  ) where


import Generics.Deriving.Base

--------------------------------------------------------------------------------
-- Generic show
--------------------------------------------------------------------------------

class GShow' f where
  gshows' :: Bool -> f a -> ShowS

instance GShow' U1 where
  gshows' _ U1 = showString ""

instance (GShow c) => GShow' (K1 i c) where
  gshows' _ (K1 a) = gshows a

-- No instances for P or Rec because gshow is only applicable to types of kind *

instance (GShow' a, Constructor c) => GShow' (M1 C c a) where
  gshows' _ c@(M1 a)  =   showString "(" . showString (conName c) 
                        . showString " "
                        . wrapRecord (gshows' (conIsRecord c) a . showString ")")
    where
      wrapRecord :: ShowS -> ShowS
      wrapRecord s | conIsRecord c = showString "{ " . s . showString " }"
      wrapRecord s | otherwise     = s

instance (Selector s, GShow' a) => GShow' (M1 S s a) where
  gshows' b s@(M1 a) | selName s == "" = gshows' b a
                     | otherwise       =   showString (selName s)
                                         . showString " = " . gshows' b a

instance (GShow' a) => GShow' (M1 D d a) where
  gshows' b (M1 a) = gshows' b a

instance (GShow' a, GShow' b) => GShow' (a :+: b) where
  gshows' b (L1 a) = gshows' b a
  gshows' b (R1 a) = gshows' b a

instance (GShow' a, GShow' b) => GShow' (a :*: b) where
  gshows' False (a :*: b) = gshows' False a . showString " " . gshows' False b
  gshows' True  (a :*: b) = gshows' True  a . showString " , " . gshows' True b


class GShow a where 
  gshows :: a -> ShowS
  gshow  :: a -> String
  gshow x = gshows x ""

#ifdef __UHC__

{-# DERIVABLE GShow gshows gshowsdefault #-}
deriving instance (GShow a) => GShow (Maybe a)

#endif

gshowsdefault :: (Representable0 a rep0, GShow' rep0) => rep0 () -> a -> ShowS
gshowsdefault rep x = gshows' False (from0 x `asTypeOf` rep)


-- Base types instances
instance GShow Char   where gshows = shows
instance GShow Int    where gshows = shows
instance GShow Float  where gshows = shows
instance GShow String where gshows = shows
instance GShow Bool   where gshows = shows


showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith _     []     s = "[]" ++ s
showListWith showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)

instance (GShow a) => GShow [a] where
  gshows = showListWith gshows

#ifndef __UHC__

instance (GShow a) => GShow (Maybe a) where
  gshows = t undefined where
    t :: (GShow a) => Rep0Maybe a () -> Maybe a -> ShowS
    t = gshowsdefault

#endif
