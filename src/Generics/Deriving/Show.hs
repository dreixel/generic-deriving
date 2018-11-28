{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase #-}
#endif

module Generics.Deriving.Show (
  -- * Generic show class
    GShow(..)

  -- * Default definition
  , gshowsPrecdefault

  -- * Internal show class
  , GShow'(..)

  ) where

import           Control.Applicative (Const, ZipList)

import           Data.Char (GeneralCategory)
import           Data.Int
import           Data.Monoid (All, Any, Dual, Product, Sum)
import qualified Data.Monoid as Monoid (First, Last)
import           Data.Version (Version)
import           Data.Word

import           Foreign.C.Types
import           Foreign.ForeignPtr (ForeignPtr)
import           Foreign.Ptr

import           Generics.Deriving.Base

import           GHC.Exts hiding (Any)

import           System.Exit (ExitCode)
import           System.IO (BufferMode, Handle, HandlePosn, IOMode, SeekMode)
import           System.IO.Error (IOErrorType)
import           System.Posix.Types

#if MIN_VERSION_base(4,4,0)
import           Data.Complex (Complex)
#endif

#if MIN_VERSION_base(4,7,0)
import           Data.Proxy (Proxy)
#endif

#if MIN_VERSION_base(4,8,0)
import           Data.Functor.Identity (Identity)
import           Data.Monoid (Alt)
import           Data.Void (Void)
import           Numeric.Natural (Natural)
#endif

#if MIN_VERSION_base(4,9,0)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as Semigroup (First, Last)
import           Data.Semigroup (Arg, Max, Min, Option, WrappedMonoid)
#endif

--------------------------------------------------------------------------------
-- Generic show
--------------------------------------------------------------------------------

intersperse :: a -> [a] -> [a]
intersperse _ []    = []
intersperse _ [h]   = [h]
intersperse x (h:t) = h : x : (intersperse x t)

appPrec :: Int
appPrec = 2

data Type = Rec | Tup | Pref | Inf String

class GShow' f where
  gshowsPrec' :: Type -> Int -> f a -> ShowS
  isNullary   :: f a -> Bool
  isNullary = error "generic show (isNullary): unnecessary case"

instance GShow' V1 where
  gshowsPrec' _ _ x = case x of
#if __GLASGOW_HASKELL__ >= 708
                        {}
#else
                        !_ -> error "Void gshowsPrec"
#endif

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

            conIsTuple :: C1 c f p -> Bool
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

-- Unboxed types
instance GShow' UChar where
  gshowsPrec' _ _ (UChar c)   = showsPrec 0 (C# c) . showChar '#'
instance GShow' UDouble where
  gshowsPrec' _ _ (UDouble d) = showsPrec 0 (D# d) . showString "##"
instance GShow' UFloat where
  gshowsPrec' _ _ (UFloat f)  = showsPrec 0 (F# f) . showChar '#'
instance GShow' UInt where
  gshowsPrec' _ _ (UInt i)    = showsPrec 0 (I# i) . showChar '#'
instance GShow' UWord where
  gshowsPrec' _ _ (UWord w)   = showsPrec 0 (W# w) . showString "##"


class GShow a where
  gshowsPrec :: Int -> a -> ShowS
#if __GLASGOW_HASKELL__ >= 701
  default gshowsPrec :: (Generic a, GShow' (Rep a))
                     => Int -> a -> ShowS
  gshowsPrec = gshowsPrecdefault
#endif

  gshows :: a -> ShowS
  gshows = gshowsPrec 0

  gshow :: a -> String
  gshow x = gshows x ""

  gshowList :: [a] -> ShowS
  gshowList l =   showChar '['
                . foldr (.) id
                   (intersperse (showChar ',') (map (gshowsPrec 0) l))
                . showChar ']'

gshowsPrecdefault :: (Generic a, GShow' (Rep a))
                  => Int -> a -> ShowS
gshowsPrecdefault n = gshowsPrec' Pref n . from


-- Base types instances
-- Base types instances
instance GShow () where
  gshowsPrec = gshowsPrecdefault

instance (GShow a, GShow b) => GShow (a, b) where
  gshowsPrec = gshowsPrecdefault

instance (GShow a, GShow b, GShow c) => GShow (a, b, c) where
  gshowsPrec = gshowsPrecdefault

instance (GShow a, GShow b, GShow c, GShow d) => GShow (a, b, c, d) where
  gshowsPrec = gshowsPrecdefault

instance (GShow a, GShow b, GShow c, GShow d, GShow e) => GShow (a, b, c, d, e) where
  gshowsPrec = gshowsPrecdefault

instance (GShow a, GShow b, GShow c, GShow d, GShow e, GShow f)
    => GShow (a, b, c, d, e, f) where
  gshowsPrec = gshowsPrecdefault

instance (GShow a, GShow b, GShow c, GShow d, GShow e, GShow f, GShow g)
    => GShow (a, b, c, d, e, f, g) where
  gshowsPrec = gshowsPrecdefault

instance GShow a => GShow [a] where
  gshowsPrec _ = gshowList

instance (GShow (f p), GShow (g p)) => GShow ((f :+: g) p) where
  gshowsPrec = gshowsPrecdefault

instance (GShow (f p), GShow (g p)) => GShow ((f :*: g) p) where
  gshowsPrec = gshowsPrecdefault

instance GShow (f (g p)) => GShow ((f :.: g) p) where
  gshowsPrec = gshowsPrecdefault

instance GShow All where
  gshowsPrec = gshowsPrecdefault

#if MIN_VERSION_base(4,8,0)
instance GShow (f a) => GShow (Alt f a) where
  gshowsPrec = gshowsPrecdefault
#endif

instance GShow Any where
  gshowsPrec = gshowsPrecdefault

#if MIN_VERSION_base(4,9,0)
instance (GShow a, GShow b) => GShow (Arg a b) where
  gshowsPrec = gshowsPrecdefault
#endif

#if !(MIN_VERSION_base(4,9,0))
instance GShow Arity where
  gshowsPrec = gshowsPrecdefault
#endif

instance GShow Associativity where
  gshowsPrec = gshowsPrecdefault

instance GShow Bool where
  gshowsPrec = gshowsPrecdefault

instance GShow BufferMode where
  gshowsPrec = showsPrec

#if defined(HTYPE_CC_T)
instance GShow CCc where
  gshowsPrec = showsPrec
#endif

instance GShow CChar where
  gshowsPrec = showsPrec

instance GShow CClock where
  gshowsPrec = showsPrec

#if defined(HTYPE_DEV_T)
instance GShow CDev where
  gshowsPrec = showsPrec
#endif

instance GShow CDouble where
  gshowsPrec = showsPrec

instance GShow CFloat where
  gshowsPrec = showsPrec

#if defined(HTYPE_GID_T)
instance GShow CGid where
  gshowsPrec = showsPrec
#endif

instance GShow Char where
  gshowsPrec = showsPrec
  gshowList  = showList

#if defined(HTYPE_INO_T)
instance GShow CIno where
  gshowsPrec = showsPrec
#endif

instance GShow CInt where
  gshowsPrec = showsPrec

instance GShow CIntMax where
  gshowsPrec = showsPrec

instance GShow CIntPtr where
  gshowsPrec = showsPrec

instance GShow CLLong where
  gshowsPrec = showsPrec

instance GShow CLong where
  gshowsPrec = showsPrec

#if defined(HTYPE_MODE_T)
instance GShow CMode where
  gshowsPrec = showsPrec
#endif

#if defined(HTYPE_NLINK_T)
instance GShow CNlink where
  gshowsPrec = showsPrec
#endif

#if defined(HTYPE_OFF_T)
instance GShow COff where
  gshowsPrec = showsPrec
#endif

#if MIN_VERSION_base(4,4,0)
instance GShow a => GShow (Complex a) where
  gshowsPrec = gshowsPrecdefault
#endif

instance GShow a => GShow (Const a b) where
  gshowsPrec = gshowsPrecdefault

#if defined(HTYPE_PID_T)
instance GShow CPid where
  gshowsPrec = showsPrec
#endif

instance GShow CPtrdiff where
  gshowsPrec = showsPrec

#if defined(HTYPE_RLIM_T)
instance GShow CRLim where
  gshowsPrec = showsPrec
#endif

instance GShow CSChar where
  gshowsPrec = showsPrec

#if defined(HTYPE_SPEED_T)
instance GShow CSpeed where
  gshowsPrec = showsPrec
#endif

#if MIN_VERSION_base(4,4,0)
instance GShow CSUSeconds where
  gshowsPrec = showsPrec
#endif

instance GShow CShort where
  gshowsPrec = showsPrec

instance GShow CSigAtomic where
  gshowsPrec = showsPrec

instance GShow CSize where
  gshowsPrec = showsPrec

#if defined(HTYPE_SSIZE_T)
instance GShow CSsize where
  gshowsPrec = showsPrec
#endif

#if defined(HTYPE_TCFLAG_T)
instance GShow CTcflag where
  gshowsPrec = showsPrec
#endif

instance GShow CTime where
  gshowsPrec = showsPrec

instance GShow CUChar where
  gshowsPrec = showsPrec

#if defined(HTYPE_UID_T)
instance GShow CUid where
  gshowsPrec = showsPrec
#endif

instance GShow CUInt where
  gshowsPrec = showsPrec

instance GShow CUIntMax where
  gshowsPrec = showsPrec

instance GShow CUIntPtr where
  gshowsPrec = showsPrec

instance GShow CULLong where
  gshowsPrec = showsPrec

instance GShow CULong where
  gshowsPrec = showsPrec

#if MIN_VERSION_base(4,4,0)
instance GShow CUSeconds where
  gshowsPrec = showsPrec
#endif

instance GShow CUShort where
  gshowsPrec = showsPrec

instance GShow CWchar where
  gshowsPrec = showsPrec

instance GShow Double where
  gshowsPrec = showsPrec

instance GShow a => GShow (Down a) where
  gshowsPrec = gshowsPrecdefault

instance GShow a => GShow (Dual a) where
  gshowsPrec = gshowsPrecdefault

instance (GShow a, GShow b) => GShow (Either a b) where
  gshowsPrec = gshowsPrecdefault

instance GShow ExitCode where
  gshowsPrec = gshowsPrecdefault

instance GShow Fd where
  gshowsPrec = showsPrec

instance GShow a => GShow (Monoid.First a) where
  gshowsPrec = gshowsPrecdefault

#if MIN_VERSION_base(4,9,0)
instance GShow a => GShow (Semigroup.First a) where
  gshowsPrec = gshowsPrecdefault
#endif

instance GShow Fixity where
  gshowsPrec = gshowsPrecdefault

instance GShow Float where
  gshowsPrec = showsPrec

instance GShow (ForeignPtr a) where
  gshowsPrec = showsPrec

instance GShow (FunPtr a) where
  gshowsPrec = showsPrec

instance GShow GeneralCategory where
  gshowsPrec = showsPrec

instance GShow Handle where
  gshowsPrec = showsPrec

instance GShow HandlePosn where
  gshowsPrec = showsPrec

#if MIN_VERSION_base(4,8,0)
instance GShow a => GShow (Identity a) where
  gshowsPrec = gshowsPrecdefault
#endif

instance GShow Int where
  gshowsPrec = showsPrec

instance GShow Int8 where
  gshowsPrec = showsPrec

instance GShow Int16 where
  gshowsPrec = showsPrec

instance GShow Int32 where
  gshowsPrec = showsPrec

instance GShow Int64 where
  gshowsPrec = showsPrec

instance GShow Integer where
  gshowsPrec = showsPrec

instance GShow IntPtr where
  gshowsPrec = showsPrec

instance GShow IOError where
  gshowsPrec = showsPrec

instance GShow IOErrorType where
  gshowsPrec = showsPrec

instance GShow IOMode where
  gshowsPrec = showsPrec

instance GShow c => GShow (K1 i c p) where
  gshowsPrec = gshowsPrecdefault

instance GShow a => GShow (Monoid.Last a) where
  gshowsPrec = gshowsPrecdefault

#if MIN_VERSION_base(4,9,0)
instance GShow a => GShow (Semigroup.Last a) where
  gshowsPrec = gshowsPrecdefault
#endif

instance GShow (f p) => GShow (M1 i c f p) where
  gshowsPrec = gshowsPrecdefault

#if MIN_VERSION_base(4,9,0)
instance GShow a => GShow (Max a) where
  gshowsPrec = gshowsPrecdefault
#endif

instance GShow a => GShow (Maybe a) where
  gshowsPrec = gshowsPrecdefault

#if MIN_VERSION_base(4,9,0)
instance GShow a => GShow (Min a) where
  gshowsPrec = gshowsPrecdefault
#endif

#if MIN_VERSION_base(4,8,0)
instance GShow Natural where
  gshowsPrec = showsPrec
#endif

#if MIN_VERSION_base(4,9,0)
instance GShow a => GShow (NonEmpty a) where
  gshowsPrec = gshowsPrecdefault

instance GShow a => GShow (Option a) where
  gshowsPrec = gshowsPrecdefault
#endif

instance GShow Ordering where
  gshowsPrec = gshowsPrecdefault

instance GShow p => GShow (Par1 p) where
  gshowsPrec = gshowsPrecdefault

instance GShow a => GShow (Product a) where
  gshowsPrec = gshowsPrecdefault

#if MIN_VERSION_base(4,7,0)
instance GShow (Proxy s) where
  gshowsPrec = gshowsPrecdefault
#endif

instance GShow (Ptr a) where
  gshowsPrec = showsPrec

instance GShow (f p) => GShow (Rec1 f p) where
  gshowsPrec = gshowsPrecdefault

instance GShow SeekMode where
  gshowsPrec = showsPrec

instance GShow a => GShow (Sum a) where
  gshowsPrec = gshowsPrecdefault

instance GShow (U1 p) where
  gshowsPrec = gshowsPrecdefault

instance GShow (UChar p) where
  gshowsPrec = gshowsPrecdefault

instance GShow (UDouble p) where
  gshowsPrec = gshowsPrecdefault

instance GShow (UFloat p) where
  gshowsPrec = gshowsPrecdefault

instance GShow (UInt p) where
  gshowsPrec = gshowsPrecdefault

instance GShow (UWord p) where
  gshowsPrec = gshowsPrecdefault

instance GShow Version where
  gshowsPrec = gshowsPrecdefault

#if MIN_VERSION_base(4,8,0)
instance GShow Void where
  gshowsPrec = showsPrec
#endif

instance GShow Word where
  gshowsPrec = showsPrec

instance GShow Word8 where
  gshowsPrec = showsPrec

instance GShow Word16 where
  gshowsPrec = showsPrec

instance GShow Word32 where
  gshowsPrec = showsPrec

instance GShow Word64 where
  gshowsPrec = showsPrec

instance GShow WordPtr where
  gshowsPrec = showsPrec

#if MIN_VERSION_base(4,9,0)
instance GShow m => GShow (WrappedMonoid m) where
  gshowsPrec = gshowsPrecdefault
#endif

instance GShow a => GShow (ZipList a) where
  gshowsPrec = gshowsPrecdefault

#if MIN_VERSION_base(4,10,0)
instance GShow CBool where
  gshowsPrec = showsPrec

# if defined(HTYPE_BLKSIZE_T)
instance GShow CBlkSize where
  gshowsPrec = showsPrec
# endif

# if defined(HTYPE_BLKCNT_T)
instance GShow CBlkCnt where
  gshowsPrec = showsPrec
# endif

# if defined(HTYPE_CLOCKID_T)
instance GShow CClockId where
  gshowsPrec = showsPrec
# endif

# if defined(HTYPE_FSBLKCNT_T)
instance GShow CFsBlkCnt where
  gshowsPrec = showsPrec
# endif

# if defined(HTYPE_FSFILCNT_T)
instance GShow CFsFilCnt where
  gshowsPrec = showsPrec
# endif

# if defined(HTYPE_ID_T)
instance GShow CId where
  gshowsPrec = showsPrec
# endif

# if defined(HTYPE_KEY_T)
instance GShow CKey where
  gshowsPrec = showsPrec
# endif

# if defined(HTYPE_TIMER_T)
instance GShow CTimer where
  gshowsPrec = showsPrec
# endif
#endif
