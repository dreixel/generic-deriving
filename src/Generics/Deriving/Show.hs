{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

module Generics.Deriving.Show (
  -- * Generic show class
    GShow(..)

  -- * Default definition
  , gshowsPrecdefault

  ) where


import Generics.Deriving.Base
import Generics.Deriving.Instances ()

--------------------------------------------------------------------------------
-- Generic show
--------------------------------------------------------------------------------

appPrec :: Int
appPrec = 2

data Type = Rec | Tup | Pref | Inf String

class GShow' f where
  gshowsPrec' :: Type -> Int -> f a -> ShowS
  isNullary   :: f a -> Bool
  isNullary = error "generic show (isNullary): unnecessary case"

instance GShow' U1 where
  gshowsPrec' _ _ U1 = id
  isNullary _ = True

instance (GShow c) => GShow' (K1 i c) where
  gshowsPrec' _ n (K1 a) = gshowsPrec n a
  isNullary _ = False

-- No instances for P or Rec because gshow is only applicable to types of kind *

instance (GShow' a, Constructor c) => GShow' (M1 C c a) where
  gshowsPrec' _ n c@(M1 x) = 
    case fixity of
      Prefix    -> showParen (n > appPrec && not (isNullary x)) 
                    ( showString (conName c) 
                    . if (isNullary x) then id else showChar ' '
                    . showBraces t (gshowsPrec' t appPrec x))
      Infix _ m -> showParen (n > m) (showBraces t (gshowsPrec' t m x))
      where fixity = conFixity c
            t = if (conIsRecord c) then Rec else
                  case (conIsTuple c) of
                    True -> Tup
                    False -> case fixity of
                                Prefix    -> Pref
                                Infix _ _ -> Inf (show (conName c))
            showBraces :: Type -> ShowS -> ShowS
            showBraces Rec     p = showChar '{' . p . showChar '}'
            showBraces Tup     p = showChar '(' . p . showChar ')'
            showBraces Pref    p = p
            showBraces (Inf _) p = p
            conIsTuple y = tupleName (conName y) where
              tupleName ('(':',':_) = True
              tupleName _           = False

instance (Selector s, GShow' a) => GShow' (M1 S s a) where
  gshowsPrec' t n s@(M1 x) | selName s == "" = --showParen (n > appPrec)
                                                 (gshowsPrec' t n x)
                           | otherwise       =   showString (selName s)
                                               . showString " = "
                                               . gshowsPrec' t 0 x
  isNullary (M1 x) = isNullary x

instance (GShow' a) => GShow' (M1 D d a) where
  gshowsPrec' t n (M1 x) = gshowsPrec' t n x

instance (GShow' a, GShow' b) => GShow' (a :+: b) where
  gshowsPrec' t n (L1 x) = gshowsPrec' t n x
  gshowsPrec' t n (R1 x) = gshowsPrec' t n x

instance (GShow' a, GShow' b) => GShow' (a :*: b) where
  gshowsPrec' t@Rec     n (a :*: b) =
    gshowsPrec' t n     a . showString ", " . gshowsPrec' t n     b
  gshowsPrec' t@(Inf s) n (a :*: b) =
    gshowsPrec' t n     a . showString s    . gshowsPrec' t n     b
  gshowsPrec' t@Tup     n (a :*: b) =
    gshowsPrec' t n     a . showChar ','    . gshowsPrec' t n     b
  gshowsPrec' t@Pref    n (a :*: b) =
    gshowsPrec' t (n+1) a . showChar ' '    . gshowsPrec' t (n+1) b
  
  -- If we have a product then it is not a nullary constructor
  isNullary _ = False


class GShow a where 
  gshowsPrec :: Int -> a -> ShowS
  gshows :: a -> ShowS
  gshows = gshowsPrec 0
  gshow :: a -> String
  gshow x = gshows x ""
#if __GLASGOW_HASKELL__ >= 701
  default gshowsPrec :: (Generic a, GShow' (Rep a))
                     => Int -> a -> ShowS
  gshowsPrec = gshowsPrecdefault

instance (GShow a) => GShow (Maybe a)

#else

instance (GShow a) => GShow (Maybe a) where
  gshowsPrec = gshowsPrecdefault

#endif

gshowsPrecdefault :: (Generic a, GShow' (Rep a))
                  => Int -> a -> ShowS
gshowsPrecdefault n = gshowsPrec' Pref n . from


-- Base types instances
instance GShow Char   where gshowsPrec = showsPrec
instance GShow Int    where gshowsPrec = showsPrec
instance GShow Float  where gshowsPrec = showsPrec
instance GShow String where gshowsPrec = showsPrec
instance GShow Bool   where gshowsPrec = showsPrec

intersperse :: a -> [a] -> [a]
intersperse _ []    = []
intersperse _ [h]   = [h]
intersperse x (h:t) = h : x : (intersperse x t)

instance (GShow a) => GShow [a] where
  gshowsPrec _ l =   showChar '['
                   . foldr (.) id
                      (intersperse (showChar ',') (map (gshowsPrec 0) l))
                   . showChar ']'
