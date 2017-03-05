{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 705
{-# LANGUAGE PolyKinds #-}
#endif

#include "HsBaseConfig.h"

module Generics.Deriving.Enum (

  -- * Generic enum class
    GEnum(..)

  -- * Default definitions for GEnum
  , genumDefault, toEnumDefault, fromEnumDefault

  -- * Internal enum class
  , Enum'(..)

  -- * Generic Ix class
  , GIx(..)

  -- * Default definitions for GIx
  , rangeDefault, indexDefault, inRangeDefault

  ) where

import           Control.Applicative (Const, ZipList)

import           Data.Int
import           Data.Monoid (All, Any, Dual, Product, Sum)
import qualified Data.Monoid as Monoid (First, Last)
import Data.Word

import           Foreign.C.Types
import           Foreign.Ptr

import           Generics.Deriving.Base
import           Generics.Deriving.Eq

import           System.Exit (ExitCode)
import           System.Posix.Types

#if MIN_VERSION_base(4,4,0)
import           Data.Complex (Complex)
#endif

#if MIN_VERSION_base(4,7,0)
import           Data.Coerce (coerce)
import           Data.Proxy (Proxy)
#else
import           Unsafe.Coerce (unsafeCoerce)
#endif

#if MIN_VERSION_base(4,8,0)
import           Data.Functor.Identity (Identity)
import           Data.Monoid (Alt)
import           Numeric.Natural (Natural)
#endif

#if MIN_VERSION_base(4,9,0)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as Semigroup (First, Last)
import           Data.Semigroup (Arg, Max, Min, Option, WrappedMonoid)
#endif

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

genumNumUnbounded :: Num a => [a]
genumNumUnbounded = pos 0 ||| neg 0 where
  pos n = n     : pos (n + 1)
  neg n = (n-1) : neg (n - 1)

genumNumSigned :: (Bounded a, Enum a, Num a) => [a]
genumNumSigned = [0 .. maxBound] ||| [-1, -2 .. minBound]

genumNumUnsigned :: (Enum a, Num a) => [a]
genumNumUnsigned = [0 ..]

#if !(MIN_VERSION_base(4,7,0))
coerce :: a -> b
coerce = unsafeCoerce
#endif

-- Base types instances
instance GEnum () where
  genum = genumDefault

instance (GEnum a, GEnum b) => GEnum (a, b) where
  genum = genumDefault

instance (GEnum a, GEnum b, GEnum c) => GEnum (a, b, c) where
  genum = genumDefault

instance (GEnum a, GEnum b, GEnum c, GEnum d) => GEnum (a, b, c, d) where
  genum = genumDefault

instance (GEnum a, GEnum b, GEnum c, GEnum d, GEnum e) => GEnum (a, b, c, d, e) where
  genum = genumDefault

instance (GEnum a, GEnum b, GEnum c, GEnum d, GEnum e, GEnum f)
    => GEnum (a, b, c, d, e, f) where
  genum = genumDefault

instance (GEnum a, GEnum b, GEnum c, GEnum d, GEnum e, GEnum f, GEnum g)
    => GEnum (a, b, c, d, e, f, g) where
  genum = genumDefault

instance GEnum a => GEnum [a] where
  genum = genumDefault

instance (GEnum (f p), GEnum (g p)) => GEnum ((f :+: g) p) where
  genum = genumDefault

instance (GEnum (f p), GEnum (g p)) => GEnum ((f :*: g) p) where
  genum = genumDefault

instance GEnum (f (g p)) => GEnum ((f :.: g) p) where
  genum = genumDefault

instance GEnum All where
  genum = genumDefault

#if MIN_VERSION_base(4,8,0)
instance GEnum (f a) => GEnum (Alt f a) where
  genum = genumDefault
#endif

instance GEnum Any where
  genum = genumDefault

#if MIN_VERSION_base(4,9,0)
instance (GEnum a, GEnum b) => GEnum (Arg a b) where
  genum = genumDefault
#endif

#if !(MIN_VERSION_base(4,9,0))
instance GEnum Arity where
  genum = genumDefault
#endif

instance GEnum Associativity where
  genum = genumDefault

instance GEnum Bool where
  genum = genumDefault

#if defined(HTYPE_CC_T)
instance GEnum CCc where
  genum = coerce (genum :: [HTYPE_CC_T])
#endif

instance GEnum CChar where
  genum = coerce (genum :: [HTYPE_CHAR])

instance GEnum CClock where
  genum = coerce (genum :: [HTYPE_CLOCK_T])

#if defined(HTYPE_DEV_T)
instance GEnum CDev where
  genum = coerce (genum :: [HTYPE_DEV_T])
#endif

instance GEnum CDouble where
  genum = coerce (genum :: [HTYPE_DOUBLE])

instance GEnum CFloat where
  genum = coerce (genum :: [HTYPE_FLOAT])

#if defined(HTYPE_GID_T)
instance GEnum CGid where
  genum = coerce (genum :: [HTYPE_GID_T])
#endif

#if defined(HTYPE_INO_T)
instance GEnum CIno where
  genum = coerce (genum :: [HTYPE_INO_T])
#endif

instance GEnum CInt where
  genum = coerce (genum :: [HTYPE_INT])

instance GEnum CIntMax where
  genum = coerce (genum :: [HTYPE_INTMAX_T])

instance GEnum CIntPtr where
  genum = coerce (genum :: [HTYPE_INTPTR_T])

instance GEnum CLLong where
  genum = coerce (genum :: [HTYPE_LONG_LONG])

instance GEnum CLong where
  genum = coerce (genum :: [HTYPE_LONG])

#if defined(HTYPE_MODE_T)
instance GEnum CMode where
  genum = coerce (genum :: [HTYPE_MODE_T])
#endif

#if defined(HTYPE_NLINK_T)
instance GEnum CNlink where
  genum = coerce (genum :: [HTYPE_NLINK_T])
#endif

#if defined(HTYPE_OFF_T)
instance GEnum COff where
  genum = coerce (genum :: [HTYPE_OFF_T])
#endif

#if MIN_VERSION_base(4,4,0)
instance GEnum a => GEnum (Complex a) where
  genum = genumDefault
#endif

instance GEnum a => GEnum (Const a b) where
  genum = genumDefault

#if defined(HTYPE_PID_T)
instance GEnum CPid where
  genum = coerce (genum :: [HTYPE_PID_T])
#endif

instance GEnum CPtrdiff where
  genum = coerce (genum :: [HTYPE_PTRDIFF_T])

#if defined(HTYPE_RLIM_T)
instance GEnum CRLim where
  genum = coerce (genum :: [HTYPE_RLIM_T])
#endif

instance GEnum CSChar where
  genum = coerce (genum :: [HTYPE_SIGNED_CHAR])

#if defined(HTYPE_SPEED_T)
instance GEnum CSpeed where
  genum = coerce (genum :: [HTYPE_SPEED_T])
#endif

#if MIN_VERSION_base(4,4,0)
instance GEnum CSUSeconds where
  genum = coerce (genum :: [HTYPE_SUSECONDS_T])
#endif

instance GEnum CShort where
  genum = coerce (genum :: [HTYPE_SHORT])

instance GEnum CSigAtomic where
  genum = coerce (genum :: [HTYPE_SIG_ATOMIC_T])

instance GEnum CSize where
  genum = coerce (genum :: [HTYPE_SIZE_T])

#if defined(HTYPE_SSIZE_T)
instance GEnum CSsize where
  genum = coerce (genum :: [HTYPE_SSIZE_T])
#endif

#if defined(HTYPE_TCFLAG_T)
instance GEnum CTcflag where
  genum = coerce (genum :: [HTYPE_TCFLAG_T])
#endif

instance GEnum CTime where
  genum = coerce (genum :: [HTYPE_TIME_T])

instance GEnum CUChar where
  genum = coerce (genum :: [HTYPE_UNSIGNED_CHAR])

#if defined(HTYPE_UID_T)
instance GEnum CUid where
  genum = coerce (genum :: [HTYPE_UID_T])
#endif

instance GEnum CUInt where
  genum = coerce (genum :: [HTYPE_UNSIGNED_INT])

instance GEnum CUIntMax where
  genum = coerce (genum :: [HTYPE_UINTMAX_T])

instance GEnum CUIntPtr where
  genum = coerce (genum :: [HTYPE_UINTPTR_T])

instance GEnum CULLong where
  genum = coerce (genum :: [HTYPE_UNSIGNED_LONG_LONG])

instance GEnum CULong where
  genum = coerce (genum :: [HTYPE_UNSIGNED_LONG])

#if MIN_VERSION_base(4,4,0)
instance GEnum CUSeconds where
  genum = coerce (genum :: [HTYPE_USECONDS_T])
#endif

instance GEnum CUShort where
  genum = coerce (genum :: [HTYPE_UNSIGNED_SHORT])

instance GEnum CWchar where
  genum = coerce (genum :: [HTYPE_WCHAR_T])

instance GEnum Double where
  genum = genumNumUnbounded

instance GEnum a => GEnum (Dual a) where
  genum = genumDefault

instance (GEnum a, GEnum b) => GEnum (Either a b) where
  genum = genumDefault

instance GEnum ExitCode where
  genum = genumDefault

instance GEnum Fd where
  genum = coerce (genum :: [CInt])

instance GEnum a => GEnum (Monoid.First a) where
  genum = genumDefault

#if MIN_VERSION_base(4,9,0)
instance GEnum a => GEnum (Semigroup.First a) where
  genum = genumDefault
#endif

instance GEnum Fixity where
  genum = genumDefault

instance GEnum Float where
  genum = genumNumUnbounded

#if MIN_VERSION_base(4,8,0)
instance GEnum a => GEnum (Identity a) where
  genum = genumDefault
#endif

instance GEnum Int where
  genum = genumNumSigned

instance GEnum Int8 where
  genum = genumNumSigned

instance GEnum Int16 where
  genum = genumNumSigned

instance GEnum Int32 where
  genum = genumNumSigned

instance GEnum Int64 where
  genum = genumNumSigned

instance GEnum Integer where
  genum = genumNumUnbounded

instance GEnum IntPtr where
  genum = genumNumSigned

instance GEnum c => GEnum (K1 i c p) where
  genum = genumDefault

instance GEnum a => GEnum (Monoid.Last a) where
  genum = genumDefault

#if MIN_VERSION_base(4,9,0)
instance GEnum a => GEnum (Semigroup.Last a) where
  genum = genumDefault
#endif

instance GEnum (f p) => GEnum (M1 i c f p) where
  genum = genumDefault

#if MIN_VERSION_base(4,9,0)
instance GEnum a => GEnum (Max a) where
  genum = genumDefault
#endif

instance GEnum a => GEnum (Maybe a) where
  genum = genumDefault

#if MIN_VERSION_base(4,9,0)
instance GEnum a => GEnum (Min a) where
  genum = genumDefault
#endif

#if MIN_VERSION_base(4,8,0)
instance GEnum Natural where
  genum = genumNumUnsigned
#endif

#if MIN_VERSION_base(4,9,0)
instance GEnum a => GEnum (NonEmpty a) where
  genum = genumDefault

instance GEnum a => GEnum (Option a) where
  genum = genumDefault
#endif

instance GEnum Ordering where
  genum = genumDefault

instance GEnum p => GEnum (Par1 p) where
  genum = genumDefault

instance GEnum a => GEnum (Product a) where
  genum = genumDefault

#if MIN_VERSION_base(4,7,0)
instance GEnum
# if MIN_VERSION_base(4,9,0)
               (Proxy s)
# else
               (Proxy (s :: *))
# endif
               where
  genum = genumDefault
#endif

instance GEnum (f p) => GEnum (Rec1 f p) where
  genum = genumDefault

instance GEnum a => GEnum (Sum a) where
  genum = genumDefault

instance GEnum (U1 p) where
  genum = genumDefault

instance GEnum Word where
  genum = genumNumUnsigned

instance GEnum Word8 where
  genum = genumNumUnsigned

instance GEnum Word16 where
  genum = genumNumUnsigned

instance GEnum Word32 where
  genum = genumNumUnsigned

instance GEnum Word64 where
  genum = genumNumUnsigned

instance GEnum WordPtr where
  genum = genumNumUnsigned

#if MIN_VERSION_base(4,9,0)
instance GEnum m => GEnum (WrappedMonoid m) where
  genum = genumDefault
#endif

instance GEnum a => GEnum (ZipList a) where
  genum = genumDefault

#if MIN_VERSION_base(4,10,0)
instance GEnum CBool where
  genum = coerce (genum :: [HTYPE_BOOL])

# if defined(HTYPE_BLKSIZE_T)
instance GEnum CBlkSize where
  genum = coerce (genum :: [HTYPE_BLKSIZE_T])
# endif

# if defined(HTYPE_BLKCNT_T)
instance GEnum CBlkCnt where
  genum = coerce (genum :: [HTYPE_BLKCNT_T])
# endif

# if defined(HTYPE_CLOCKID_T)
instance GEnum CClockId where
  genum = coerce (genum :: [HTYPE_CLOCKID_T])
# endif

# if defined(HTYPE_FSBLKCNT_T)
instance GEnum CFsBlkCnt where
  genum = coerce (genum :: [HTYPE_FSBLKCNT_T])
# endif

# if defined(HTYPE_FSFILCNT_T)
instance GEnum CFsFilCnt where
  genum = coerce (genum :: [HTYPE_FSFILCNT_T])
# endif

# if defined(HTYPE_ID_T)
instance GEnum CId where
  genum = coerce (genum :: [HTYPE_ID_T])
# endif

# if defined(HTYPE_KEY_T)
instance GEnum CKey where
  genum = coerce (genum :: [HTYPE_KEY_T])
# endif
#endif

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

rangeEnum :: Enum a => (a, a) -> [a]
rangeEnum (m,n) = [m..n]

indexIntegral :: Integral a => (a, a) -> a -> Int
indexIntegral (m,_n) i = fromIntegral (i - m)

inRangeOrd :: Ord a => (a, a) -> a -> Bool
inRangeOrd (m,n) i =  m <= i && i <= n

-- Base types instances
instance GIx () where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance (GEq a, GEnum a, GIx a, GEq b, GEnum b, GIx b) => GIx (a, b) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance (GEq a, GEnum a, GIx a, GEq b, GEnum b, GIx b, GEq c, GEnum c, GIx c)
    => GIx (a, b, c) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance (GEq a, GEnum a, GIx a, GEq b, GEnum b, GIx b, GEq c, GEnum c, GIx c,
          GEq d, GEnum d, GIx d)
    => GIx (a, b, c, d) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance (GEq a, GEnum a, GIx a, GEq b, GEnum b, GIx b, GEq c, GEnum c, GIx c,
          GEq d, GEnum d, GIx d, GEq e, GEnum e, GIx e)
    => GIx (a, b, c, d, e) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance (GEq a, GEnum a, GIx a, GEq b, GEnum b, GIx b, GEq c, GEnum c, GIx c,
          GEq d, GEnum d, GIx d, GEq e, GEnum e, GIx e, GEq f, GEnum f, GIx f)
    => GIx (a, b, c, d, e, f) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance (GEq a, GEnum a, GIx a, GEq b, GEnum b, GIx b, GEq c, GEnum c, GIx c,
          GEq d, GEnum d, GIx d, GEq e, GEnum e, GIx e, GEq f, GEnum f, GIx f,
          GEq g, GEnum g, GIx g)
    => GIx (a, b, c, d, e, f, g) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance (GEq a, GEnum a, GIx a) => GIx [a] where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance GIx All where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

#if MIN_VERSION_base(4,8,0)
instance (GEq (f a), GEnum (f a), GIx (f a)) => GIx (Alt f a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

instance GIx Any where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

#if MIN_VERSION_base(4,9,0)
instance (GEq a, GEnum a, GIx a, GEnum b) => GIx (Arg a b) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

#if !(MIN_VERSION_base(4,9,0))
instance GIx Arity where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

instance GIx Associativity where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance GIx Bool where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance GIx CChar where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

#if defined(HTYPE_GID_T)
instance GIx CGid where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
#endif

#if defined(HTYPE_INO_T)
instance GIx CIno where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
#endif

instance GIx CInt where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CIntMax where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CIntPtr where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CLLong where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CLong where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

#if defined(HTYPE_MODE_T)
instance GIx CMode where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
#endif

#if defined(HTYPE_NLINK_T)
instance GIx CNlink where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
#endif

#if defined(HTYPE_OFF_T)
instance GIx COff where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
#endif

#if defined(HTYPE_PID_T)
instance GIx CPid where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
#endif

instance GIx CPtrdiff where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

#if defined(HTYPE_RLIM_T)
instance GIx CRLim where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
#endif

instance GIx CSChar where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CShort where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CSigAtomic where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CSize where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

#if defined(HTYPE_SSIZE_T)
instance GIx CSsize where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
#endif

#if defined(HTYPE_TCFLAG_T)
instance GIx CTcflag where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
#endif

instance GIx CUChar where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

#if defined(HTYPE_UID_T)
instance GIx CUid where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
#endif

instance GIx CUInt where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CUIntMax where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CUIntPtr where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CULLong where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CULong where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CUShort where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx CWchar where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance (GEq a, GEnum a, GIx a) => GIx (Dual a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance (GEq a, GEnum a, GIx a, GEq b, GEnum b, GIx b) => GIx (Either a b) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance GIx ExitCode where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance GIx Fd where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance (GEq a, GEnum a, GIx a) => GIx (Monoid.First a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

#if MIN_VERSION_base(4,9,0)
instance (GEq a, GEnum a, GIx a) => GIx (Semigroup.First a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

instance GIx Fixity where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

#if MIN_VERSION_base(4,8,0)
instance (GEq a, GEnum a, GIx a) => GIx (Identity a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

instance GIx Int where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx Int8 where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx Int16 where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx Int32 where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx Int64 where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx Integer where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx IntPtr where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance (GEq a, GEnum a, GIx a) => GIx (Monoid.Last a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

#if MIN_VERSION_base(4,9,0)
instance (GEq a, GEnum a, GIx a) => GIx (Semigroup.Last a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

#if MIN_VERSION_base(4,9,0)
instance (GEq a, GEnum a, GIx a) => GIx (Max a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

instance (GEq a, GEnum a, GIx a) => GIx (Maybe a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

#if MIN_VERSION_base(4,9,0)
instance (GEq a, GEnum a, GIx a) => GIx (Min a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

#if MIN_VERSION_base(4,8,0)
instance GIx Natural where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
#endif

#if MIN_VERSION_base(4,9,0)
instance (GEq a, GEnum a, GIx a) => GIx (NonEmpty a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

#if MIN_VERSION_base(4,9,0)
instance (GEq a, GEnum a, GIx a) => GIx (Option a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

instance GIx Ordering where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance (GEq a, GEnum a, GIx a) => GIx (Product a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

#if MIN_VERSION_base(4,7,0)
instance GIx
# if MIN_VERSION_base(4,9,0)
             (Proxy s)
# else
             (Proxy (s :: *))
# endif
             where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

instance (GEq a, GEnum a, GIx a) => GIx (Sum a) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault

instance GIx Word where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx Word8 where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx Word16 where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx Word32 where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx Word64 where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

instance GIx WordPtr where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

#if MIN_VERSION_base(4,9,0)
instance (GEq m, GEnum m, GIx m) => GIx (WrappedMonoid m) where
  range   = rangeDefault
  index   = indexDefault
  inRange = inRangeDefault
#endif

#if MIN_VERSION_base(4,10,0)
instance GIx CBool where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd

# if defined(HTYPE_BLKSIZE_T)
instance GIx CBlkSize where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
# endif

# if defined(HTYPE_BLKCNT_T)
instance GIx CBlkCnt where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
# endif

# if defined(HTYPE_CLOCKID_T)
instance GIx CClockId where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
# endif

# if defined(HTYPE_FSBLKCNT_T)
instance GIx CFsBlkCnt where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
# endif

# if defined(HTYPE_FSFILCNT_T)
instance GIx CFsFilCnt where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
# endif

# if defined(HTYPE_ID_T)
instance GIx CId where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
# endif

# if defined(HTYPE_KEY_T)
instance GIx CKey where
  range   = rangeEnum
  index   = indexIntegral
  inRange = inRangeOrd
# endif
#endif
