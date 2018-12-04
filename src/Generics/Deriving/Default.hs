{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Deriving.Default
  ( Default(..)
  ) where

import Generics.Deriving.Base
import Generics.Deriving.Enum
import Generics.Deriving.Eq
import Generics.Deriving.Show

newtype Default a = Default a

--------------------------------------------------------------------------------
-- Eq
--------------------------------------------------------------------------------

instance (Generic a, GEq' (Rep a)) => Eq (Default a) where
  Default x == Default y = x `geqdefault` y

instance (Generic a, GEq' (Rep a)) => GEq (Default a) where
  Default x `geq` Default y = x `geqdefault` y

--------------------------------------------------------------------------------
-- Enum
--------------------------------------------------------------------------------

instance (Generic a, GEq a, Enum' (Rep a)) => Enum (Default a) where
  toEnum = Default . toEnumDefault
  fromEnum (Default x) = fromEnumDefault x

instance (Generic a, GEq a, Enum' (Rep a)) => GEnum (Default a) where
  genum = Default . to <$> enum'

--------------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------------

instance (Generic a, GShow' (Rep a)) => Show (Default a) where
  showsPrec n (Default x) = gshowsPrecdefault n x

instance (Generic a, GShow' (Rep a)) => GShow (Default a) where
  gshowsPrec n (Default x) = gshowsPrecdefault n x

