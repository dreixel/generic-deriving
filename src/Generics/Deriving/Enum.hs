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
import Generics.Deriving.Eq


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

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p xs = let l = [ i | (y,i) <- zip xs [(0::Int)..], p y]
                 in if (null l)
                    then Nothing
                    else Just (head l)

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
deriving instance (GEnum a) => GEnum [a]

{-# DERIVABLE Enum toEnum toEnumDefault #-}
{-# DERIVABLE Enum fromEnum fromEnumDefault #-}

#else

instance (GEnum a) => GEnum (Maybe a) where
  genum = t undefined where
    t :: (GEnum a) => Rep0Maybe a x -> [Maybe a]
    t = genumDefault

instance (GEnum a) => GEnum [a] where
  genum = t undefined where
    t :: (GEnum a) => Rep0List a x -> [[a]]
    t = genumDefault

#endif

genumDefault :: (Representable0 a rep0, Enum' rep0) => rep0 x -> [a]
genumDefault rep = map to0 (enum' `asTypeOf` [rep])

toEnumDefault :: (Representable0 a rep0, Enum' rep0) => rep0 x -> Int -> a
toEnumDefault rep i = let l = enum' `asTypeOf` [rep]
                      in if (length l > i)
                         then to0 (l !! i)
                         else error "toEnum: invalid index"

fromEnumDefault :: (GEq a, Representable0 a rep0, Enum' rep0)
                => rep0 x -> a -> Int
fromEnumDefault rep x = t x (map to0 (enum' `asTypeOf` [rep])) where
  -- This weird local function is to appease EHC's type checker
  t :: GEq a => a -> [a] -> Int
  t y l = case (findIndex (geq y) l) of
            Nothing -> error "fromEnum: no corresponding index"
            Just i  -> i

{-
-- Natural definition
fromEnumDefault :: (GEq a, Representable0 a rep0, Enum' rep0)
                => rep0 x -> a -> Int
fromEnumDefault rep x = let l = map to0 (enum' `asTypeOf` [rep])
                        in case (findIndex (geq x) l) of
                             Nothing -> error "fromEnum: no corresponding index"
                             Just i  -> i
-}

class GEnum a where
  genum :: [a]

instance GEnum Int where
  genum = [0..] ||| (neg 0) where
    neg n = (n-1) : neg (n-1)
