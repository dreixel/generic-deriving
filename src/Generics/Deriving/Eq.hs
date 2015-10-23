{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MagicHash #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

#include "HsBaseConfig.h"

module Generics.Deriving.Eq (
  -- * Generic show class
    GEq(..)

  -- * Default definition
  , geqdefault

  ) where

import Control.Applicative (Const, ZipList)

import Data.Char (GeneralCategory)
import Data.Complex (Complex(..))
import Data.Int
import Data.Monoid (All, Any, Dual, First, Last, Product, Sum)
import Data.Word

import Foreign.C.Error
import Foreign.C.Types
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr
import Foreign.StablePtr (StablePtr)

import Generics.Deriving.Base
import Generics.Deriving.Instances ()

import GHC.Exts hiding (Any)
import GHC.Real (Ratio(..))

import System.Exit (ExitCode)
import System.IO (BufferMode, Handle, HandlePosn, IOMode, SeekMode)
import System.IO.Error (IOErrorType)
import System.Posix.Types

#if MIN_VERSION_base(4,7,0)
import Data.Proxy (Proxy)
#endif

#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity (Identity)
import Data.Monoid (Alt)
import Data.Void (Void)
import Numeric.Natural (Natural)
#endif

--------------------------------------------------------------------------------
-- Generic show
--------------------------------------------------------------------------------

class GEq' f where
  geq' :: f a -> f a -> Bool

instance GEq' U1 where
  geq' _ _ = True

instance (GEq c) => GEq' (K1 i c) where
  geq' (K1 a) (K1 b) = geq a b

-- No instances for P or Rec because geq is only applicable to types of kind *

instance (GEq' a) => GEq' (M1 i c a) where
  geq' (M1 a) (M1 b) = geq' a b

instance (GEq' a, GEq' b) => GEq' (a :+: b) where
  geq' (L1 a) (L1 b) = geq' a b
  geq' (R1 a) (R1 b) = geq' a b
  geq' _      _      = False

instance (GEq' a, GEq' b) => GEq' (a :*: b) where
  geq' (a1 :*: b1) (a2 :*: b2) = geq' a1 a2 && geq' b1 b2

-- Unboxed types
instance GEq' UAddr where
  geq' (UAddr a1) (UAddr a2)     = isTrue# (eqAddr# a1 a2)
instance GEq' UChar where
  geq' (UChar c1) (UChar c2)     = isTrue# (eqChar# c1 c2)
instance GEq' UDouble where
  geq' (UDouble d1) (UDouble d2) = isTrue# (d1 ==## d2)
instance GEq' UFloat where
  geq' (UFloat f1) (UFloat f2)   = isTrue# (eqFloat# f1 f2)
instance GEq' UInt where
  geq' (UInt i1) (UInt i2)       = isTrue# (i1 ==# i2)
instance GEq' UWord where
  geq' (UWord w1) (UWord w2)     = isTrue# (eqWord# w1 w2)

#if __GLASGOW_HASKELL__ < 707
isTrue# :: Bool -> Bool
isTrue# = id
#endif


class GEq a where
  geq :: a -> a -> Bool


#if __GLASGOW_HASKELL__ >= 701
  default geq :: (Generic a, GEq' (Rep a)) => a -> a -> Bool
  geq = geqdefault
#endif

geqdefault :: (Generic a, GEq' (Rep a)) => a -> a -> Bool
geqdefault x y = geq' (from x) (from y)

-- Base types instances
instance GEq () where
  geq = geqdefault

instance (GEq a, GEq b) => GEq (a, b) where
  geq = geqdefault

instance (GEq a, GEq b, GEq c) => GEq (a, b, c) where
  geq = geqdefault

instance (GEq a, GEq b, GEq c, GEq d) => GEq (a, b, c, d) where
  geq = geqdefault

instance (GEq a, GEq b, GEq c, GEq d, GEq e) => GEq (a, b, c, d, e) where
  geq = geqdefault

instance (GEq a, GEq b, GEq c, GEq d, GEq e, GEq f)
    => GEq (a, b, c, d, e, f) where
  geq = geqdefault

instance (GEq a, GEq b, GEq c, GEq d, GEq e, GEq f, GEq g)
    => GEq (a, b, c, d, e, f, g) where
  geq = geqdefault

instance GEq a => GEq [a] where
  geq = geqdefault

instance (GEq (f p), GEq (g p)) => GEq ((f :+: g) p) where
  geq = geqdefault

instance (GEq (f p), GEq (g p)) => GEq ((f :*: g) p) where
  geq = geqdefault

instance GEq (f (g p)) => GEq ((f :.: g) p) where
  geq = geqdefault

instance GEq All where
  geq = geqdefault

#if MIN_VERSION_base(4,8,0)
instance GEq (f a) => GEq (Alt f a) where
  geq = geqdefault
#endif

instance GEq Any where
  geq = geqdefault

instance GEq Arity where
  geq = geqdefault

instance GEq Associativity where
  geq = geqdefault

instance GEq Bool where
  geq = geqdefault

instance GEq BufferMode where
  geq = (==)

#if defined(HTYPE_CC_T)
instance GEq CCc where
  geq = (==)
#endif

instance GEq CChar where
  geq = (==)

instance GEq CClock where
  geq = (==)

#if defined(HTYPE_DEV_T)
instance GEq CDev where
  geq = (==)
#endif

instance GEq CDouble where
  geq = (==)

instance GEq CFloat where
  geq = (==)

#if defined(HTYPE_GID_T)
instance GEq CGid where
  geq = (==)
#endif

instance GEq Char where
  geq = (==)

#if defined(HTYPE_INO_T)
instance GEq CIno where
  geq = (==)
#endif

instance GEq CInt where
  geq = (==)

instance GEq CIntMax where
  geq = (==)

instance GEq CIntPtr where
  geq = (==)

instance GEq CLLong where
  geq = (==)

instance GEq CLong where
  geq = (==)

#if defined(HTYPE_MODE_T)
instance GEq CMode where
  geq = (==)
#endif

#if defined(HTYPE_NLINK_T)
instance GEq CNlink where
  geq = (==)
#endif

#if defined(HTYPE_OFF_T)
instance GEq COff where
  geq = (==)
#endif

instance
#if MIN_VERSION_base(4,4,0)
    GEq a
#else
    (GEq a, RealFloat a)
#endif
    => GEq (Complex a) where
  geq (a1 :+ a2) (b1 :+ b2) = geq a1 b1 && geq a2 b2

instance GEq a => GEq (Const a b) where
  geq = geqdefault

#if defined(HTYPE_PID_T)
instance GEq CPid where
  geq = (==)
#endif

instance GEq CPtrdiff where
  geq = (==)

#if defined(HTYPE_RLIM_T)
instance GEq CRLim where
  geq = (==)
#endif

instance GEq CSChar where
  geq = (==)

#if defined(HTYPE_SPEED_T)
instance GEq CSpeed where
  geq = (==)
#endif

instance GEq CSUSeconds where
  geq = (==)

instance GEq CShort where
  geq = (==)

instance GEq CSigAtomic where
  geq = (==)

instance GEq CSize where
  geq = (==)

#if defined(HTYPE_SSIZE_T)
instance GEq CSsize where
  geq = (==)
#endif

#if defined(HTYPE_TCFLAG_T)
instance GEq CTcflag where
  geq = (==)
#endif

instance GEq CTime where
  geq = (==)

instance GEq CUChar where
  geq = (==)

#if defined(HTYPE_UID_T)
instance GEq CUid where
  geq = (==)
#endif

instance GEq CUInt where
  geq = (==)

instance GEq CUIntMax where
  geq = (==)

instance GEq CUIntPtr where
  geq = (==)

instance GEq CULLong where
  geq = (==)

instance GEq CULong where
  geq = (==)

instance GEq CUSeconds where
  geq = (==)

instance GEq CUShort where
  geq = (==)

instance GEq CWchar where
  geq = (==)

instance GEq Double where
  geq = (==)

instance GEq a => GEq (Dual a) where
  geq = geqdefault

instance (GEq a, GEq b) => GEq (Either a b) where
  geq = geqdefault

instance GEq Errno where
  geq = (==)

instance GEq ExitCode where
  geq = (==)

instance GEq Fd where
  geq = (==)

instance GEq a => GEq (First a) where
  geq = geqdefault

instance GEq Fixity where
  geq = geqdefault

instance GEq Float where
  geq = (==)

instance GEq (ForeignPtr a) where
  geq = (==)

instance GEq (FunPtr a) where
  geq = (==)

instance GEq GeneralCategory where
  geq = (==)

instance GEq Handle where
  geq = (==)

instance GEq HandlePosn where
  geq = (==)

#if MIN_VERSION_base(4,8,0)
instance GEq a => GEq (Identity a) where
  geq = geqdefault
#endif

instance GEq Int where
  geq = (==)

instance GEq Int8 where
  geq = (==)

instance GEq Int16 where
  geq = (==)

instance GEq Int32 where
  geq = (==)

instance GEq Int64 where
  geq = (==)

instance GEq Integer where
  geq = (==)

instance GEq IntPtr where
  geq = (==)

instance GEq IOError where
  geq = (==)

instance GEq IOErrorType where
  geq = (==)

instance GEq IOMode where
  geq = (==)

instance GEq c => GEq (K1 i c p) where
  geq = geqdefault

instance GEq a => GEq (Last a) where
  geq = geqdefault

instance GEq (f p) => GEq (M1 i c f p) where
  geq = geqdefault

instance GEq a => GEq (Maybe a) where
  geq = geqdefault

#if MIN_VERSION_base(4,8,0)
instance GEq Natural where
  geq = (==)
#endif

instance GEq Ordering where
  geq = geqdefault

instance GEq p => GEq (Par1 p) where
  geq = geqdefault

instance GEq a => GEq (Product a) where
  geq = geqdefault

#if MIN_VERSION_base(4,7,0)
instance GEq (Proxy s) where
  geq = geqdefault
#endif

instance GEq (Ptr a) where
  geq = (==)

instance GEq (f p) => GEq (Rec1 f p) where
  geq = geqdefault

instance GEq SeekMode where
  geq = (==)

instance
#if MIN_VERSION_base(4,4,0)
    GEq a
#else
    (GEq a, Integral a)
#endif
    => GEq (Ratio a) where
  geq (a1 :% a2) (b1 :% b2) = geq a1 b1 && geq a2 b2

instance GEq (StablePtr a) where
  geq = (==)

instance GEq a => GEq (Sum a) where
  geq = geqdefault

instance GEq (U1 p) where
  geq = geqdefault

#if MIN_VERSION_base(4,8,0)
instance GEq Void where
  geq = (==)
#endif

instance GEq Word where
  geq = (==)

instance GEq Word8 where
  geq = (==)

instance GEq Word16 where
  geq = (==)

instance GEq Word32 where
  geq = (==)

instance GEq Word64 where
  geq = (==)

instance GEq WordPtr where
  geq = (==)

instance GEq a => GEq (ZipList a) where
  geq = geqdefault
