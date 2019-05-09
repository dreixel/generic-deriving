{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

module Generics.Deriving.Base (module Generics.Deriving.Base.Internal) where

import Generics.Deriving.Base.Internal
import Generics.Deriving.Instances ()
