{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

module Generics.Deriving.Copoint (
  -- * GCopoint class
    GCopoint(..), 

  -- * Default method
  gcopointdefault

  ) where

import Generics.Deriving.Base
import Generics.Deriving.Instances ()


--------------------------------------------------------------------------------
-- Generic copoint
--------------------------------------------------------------------------------

class GCopoint' t where
    gcopoint' :: t a -> Maybe a

instance GCopoint' U1 where
    gcopoint' U1 = Nothing

instance GCopoint' Par1 where
    gcopoint' (Par1 a) = Just a

instance GCopoint' (K1 i c) where
    gcopoint' _ = Nothing

instance GCopoint' f => GCopoint' (M1 i c f) where
    gcopoint' (M1 a) = gcopoint' a

instance (GCopoint' f, GCopoint' g) => GCopoint' (f :+: g) where
    gcopoint' (L1 a) = gcopoint' a
    gcopoint' (R1 a) = gcopoint' a

instance (GCopoint' f, GCopoint' g) => GCopoint' (f :*: g) where
    gcopoint' (a :*: b) = case (gcopoint' a) of 
                            Just x -> Just x
                            Nothing -> gcopoint' b

instance (GCopoint f) => GCopoint' (Rec1 f) where
    gcopoint' (Rec1 a) = Just $ gcopoint a 

instance (GCopoint f, GCopoint' g) => GCopoint' (f :.: g) where
    gcopoint' (Comp1 x) = gcopoint' . gcopoint $ x

class GCopoint d where
  gcopoint :: d a -> a
#if __GLASGOW_HASKELL__ >= 701
  default gcopoint :: (Generic1 d, GCopoint' (Rep1 d))
               => (d a -> a)
  gcopoint = gcopointdefault
#endif

gcopointdefault :: (Generic1 d, GCopoint' (Rep1 d))
            => d a -> a
gcopointdefault x = case (gcopoint' . from1 $ x) of
                      Just x' -> x'
                      Nothing -> error "Data type not copointed"

instance (Generic1 d, GCopoint' (Rep1 d)) => GCopoint d where
    


