{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Deriving.ConNames
-- Copyright   :  (c) 2012 University of Oxford
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Return the name of all the constructors of a type.
--
-----------------------------------------------------------------------------

module Generics.Deriving.ConNames (

    -- * Functionality for retrieving the names of all the possible contructors
    --   of a type
    ConNames(..), conNames

  ) where

import Generics.Deriving.Base


class ConNames f where 
    gconNames :: f a -> [String]

instance (ConNames f, ConNames g) => ConNames (f :+: g) where
    gconNames (_ :: (f :+: g) a) = gconNames (undefined :: f a) ++
                                   gconNames (undefined :: g a)

instance (ConNames f) => ConNames (D1 c f) where
    gconNames (_ :: (D1 c f) a) = gconNames (undefined :: f a)
    
instance (Constructor c) => ConNames (C1 c f) where
    gconNames x = [conName x]

-- We should never need any other instances.


-- | Return the name of all the constructors of the type of the given term.
conNames :: (Generic a, ConNames (Rep a)) => a -> [String]
conNames x = gconNames (undefined `asTypeOf` (from x))
