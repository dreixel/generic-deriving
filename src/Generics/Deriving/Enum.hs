{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

module Generics.Deriving.Enum (

  -- * Generic enum class
    GEnum(..)

  -- * Default definitions for GEnum
  , genumDefault, toEnumDefault, fromEnumDefault

  -- * Generic Ix class
  , GIx(..)

  -- * Default definitions for GIx
  , rangeDefault, indexDefault, inRangeDefault

  ) where


import Generics.Deriving.Base
import Generics.Deriving.Instances ()
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


#if __GLASGOW_HASKELL__ < 701

instance (GEnum a) => GEnum (Maybe a) where
  genum = genumDefault

instance (GEnum a) => GEnum [a] where
  genum = genumDefault

#else

instance (GEnum a) => GEnum (Maybe a)
instance (GEnum a) => GEnum [a]

#endif

genumDefault :: (Generic a, Enum' (Rep a)) => [a]
genumDefault = map to enum'

toEnumDefault :: (Generic a, Enum' (Rep a)) => Int -> a
toEnumDefault i = let l = enum'
                  in if (length l > i)
                      then to (l !! i)
                       else error "toEnum: invalid index"

fromEnumDefault :: (GEq a, Generic a, Enum' (Rep a))
                => a -> Int
fromEnumDefault x = case findIndex (geq x) (map to enum') of
      Nothing -> error "fromEnum: no corresponding index"
      Just i  -> i


class GEnum a where
  genum :: [a]

#if __GLASGOW_HASKELL__ >= 701
  default genum :: (Generic a, Enum' (Rep a)) => [a]
  genum = genumDefault
#endif

instance GEnum Int where
  genum = [0..] ||| (neg 0) where
    neg n = (n-1) : neg (n-1)

--------------------------------------------------------------------------------
-- Generic Ix
--------------------------------------------------------------------------------

-- Minimal complete instance: 'range', 'index' and 'inRange'.
class (Ord a) => GIx a where
    -- | The list of values in the subrange defined by a bounding pair.
    range               :: (a,a) -> [a]
    -- | The position of a subscript in the subrange.
    index               :: (a,a) -> a -> Int
    -- | Returns 'True' the given subscript lies in the range defined
    -- the bounding pair.
    inRange             :: (a,a) -> a -> Bool
#if __GLASGOW_HASKELL__ >= 701
    default range :: (GEq a, Generic a, Enum' (Rep a)) => (a,a) -> [a]
    range = rangeDefault

    default index :: (GEq a, Generic a, Enum' (Rep a)) => (a,a) -> a -> Int
    index = indexDefault

    default inRange :: (GEq a, Generic a, Enum' (Rep a)) => (a,a) -> a -> Bool
    inRange = inRangeDefault
#endif

rangeDefault :: (GEq a, Generic a, Enum' (Rep a))
             => (a,a) -> [a]
rangeDefault = t (map to enum') where
  t l (x,y) = 
    case (findIndex (geq x) l, findIndex (geq y) l) of
      (Nothing, _)     -> error "rangeDefault: no corresponding index"
      (_, Nothing)     -> error "rangeDefault: no corresponding index"
      (Just i, Just j) -> take (j-i) (drop i l)

indexDefault :: (GEq a, Generic a, Enum' (Rep a))
             => (a,a) -> a -> Int
indexDefault = t (map to enum') where
  t l (x,y) z =
    case (findIndex (geq x) l, findIndex (geq y) l) of
      (Nothing, _)     -> error "indexDefault: no corresponding index"
      (_, Nothing)     -> error "indexDefault: no corresponding index"
      (Just i, Just j) -> case findIndex (geq z) (take (j-i) (drop i l)) of
                            Nothing -> error "indexDefault: index out of range"
                            Just k  -> k

inRangeDefault :: (GEq a, Generic a, Enum' (Rep a))
               => (a,a) -> a -> Bool
inRangeDefault = t (map to enum') where
  t l (x,y) z = 
    case (findIndex (geq x) l, findIndex (geq y) l) of
      (Nothing, _)     -> error "indexDefault: no corresponding index"
      (_, Nothing)     -> error "indexDefault: no corresponding index"
      (Just i, Just j) -> maybe False (const True)
                            (findIndex (geq z) (take (j-i) (drop i l)))

#if __GLASGOW_HASKELL__ < 701

instance (GEq a, GEnum a, GIx a) => GIx (Maybe a) where
  range = rangeDefault
  index = indexDefault
  inRange = inRangeDefault

instance (GEq a, GEnum a, GIx a) => GIx [a] where
  range = rangeDefault
  index = indexDefault
  inRange = inRangeDefault

#else

instance (GEq a, GEnum a, GIx a) => GIx (Maybe a)
instance (GEq a, GEnum a, GIx a) => GIx [a]

#endif

instance GIx Int where
    range (m,n) = [m..n]
    index (m,_n) i = i - m
    inRange (m,n) i =  m <= i && i <= n
