{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 705
{-# LANGUAGE PolyKinds #-}
#endif

{- |
Module      :  Generics.Deriving.ConNames
Copyright   :  (c) 2012 University of Oxford
License     :  BSD3

Maintainer  :  generics@haskell.org
Stability   :  experimental
Portability :  non-portable

Summary: Return the name of all the constructors of a type.
-}

module Generics.Deriving.ConNames (

    -- * Functionality for retrieving the names of the possible contructors
    --   of a type or the constructor name of a given value
    ConNames(..), conNames, conNameOf

  ) where

import Generics.Deriving.Base


class ConNames f where
    gconNames  :: f a -> [String]
    gconNameOf :: f a -> String

instance (ConNames f, ConNames g) => ConNames (f :+: g) where
    gconNames (_ :: (f :+: g) a) = gconNames (undefined :: f a) ++
                                   gconNames (undefined :: g a)

    gconNameOf (L1 x) = gconNameOf x
    gconNameOf (R1 x) = gconNameOf x

instance (ConNames f) => ConNames (D1 c f) where
    gconNames (_ :: (D1 c f) a) = gconNames (undefined :: f a)

    gconNameOf (M1 x) = gconNameOf x

instance (Constructor c) => ConNames (C1 c f) where
    gconNames x = [conName x]

    gconNameOf x = conName x


-- We should never need any other instances.


-- | Return the name of all the constructors of the type of the given term.
conNames :: (Generic a, ConNames (Rep a)) => a -> [String]
conNames x = gconNames (undefined `asTypeOf` (from x))

-- | Return the name of the constructor of the given term
conNameOf :: (ConNames (Rep a), Generic a) => a -> String
conNameOf x = gconNameOf (from x)
