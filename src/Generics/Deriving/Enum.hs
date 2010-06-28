{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

module Generics.Deriving.Enum (

  -- * Generic enum class
    GEnum(..)

  -- * Default definition
  , genumDefault

  ) where


import Generics.Deriving.Base


-----------------------------------------------------------------------------
-- Utility functions for Enum'
-----------------------------------------------------------------------------

infixr 5 |||

-- | Interleave elements from two lists. Similar to (++), but swap left and
-- right arguments on every recursive application.
--
-- From Mark Jones' talk at AFP2008
(|||) :: [a] -> [a] -> [a]
[]     ||| ys = ys
(x:xs) ||| ys = x : ys ||| xs

-- | Diagonalization of nested lists. Ensure that some elements from every
-- sublist will be included. Handles infinite sublists.
--
-- From Mark Jones' talk at AFP2008
diag :: [[a]] -> [a]
diag = concat . foldr skew [] . map (map (\x -> [x]))

skew :: [[a]] -> [[a]] -> [[a]]
skew []     ys = ys
skew (x:xs) ys = x : combine (++) xs ys

combine :: (a -> a -> a) -> [a] -> [a] -> [a]
combine _ xs     []     = xs
combine _ []     ys     = ys
combine f (x:xs) (y:ys) = f x y : combine f xs ys

{-
intersperse :: [[a]] -> [a]
intersperse ls = let f :: [[a]] -> ([a], [[a]])
                     f [] = ([], [])
                     f ([]:ls) = f ls
                     f ((h:t):ls) = let (a,b) = f ls in (h:a,t:b)
                 in case f ls of 
                   (l,[])  -> l 
                   (l,lss) -> l ++ intersperse lss where
-}

--------------------------------------------------------------------------------
-- Generic enum
--------------------------------------------------------------------------------

class Enum' f where
  enum' :: [f a]

instance Enum' U1 where
  enum' = [U1]

instance (GEnum c) => Enum' (K1 i c) where
  enum' = map K1 genum

instance (Enum' f) => Enum' (M1 i c f) where
  enum' = map M1 enum'

instance (Enum' f, Enum' g) => Enum' (f :+: g) where
  enum' = map L1 enum' ||| map R1 enum'

instance (Enum' f, Enum' g) => Enum' (f :*: g) where
  enum' = diag [ [ x :*: y | y <- enum' ] | x <- enum' ]


#ifdef __UHC__

{-# DERIVABLE GEnum genum genumDefault #-}
deriving instance (GEnum a) => GEnum (Maybe a)

#endif

genumDefault :: (Representable0 a rep0, Enum' rep0) => rep0 a -> [a]
genumDefault rep = map to0 (enum' `asTypeOf` [rep])

class GEnum a where
  genum :: [a]

instance GEnum Int where
  genum = [0..] ||| (neg 0) where
    neg n = (n-1) : neg (n-1)
