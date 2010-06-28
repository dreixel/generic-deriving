{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Generics.Deriving.Typeable (

    typeOf0default, typeOf1default,

#ifndef __UHC__
    module Data.Typeable
#endif

  ) where


import Generics.Deriving.Base

#ifndef __UHC__
import Data.Typeable
#endif

--------------------------------------------------------------------------------
-- Typeable0
--------------------------------------------------------------------------------

#ifdef __UHC__
data TypeRep
instance Eq TypeRep where (==) = undefined

data TyCon
instance Eq TyCon where (==) = undefined

mkTyConApp  :: TyCon -> [TypeRep] -> TypeRep
mkTyConApp = undefined


mkFunTy  :: TypeRep -> TypeRep -> TypeRep
mkFunTy = undefined

splitTyConApp :: TypeRep -> (TyCon,[TypeRep])
splitTyConApp = undefined

funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep
funResultTy = undefined

mkAppTy :: TypeRep -> TypeRep -> TypeRep
mkAppTy = undefined

mkTyCon :: String -> TyCon
mkTyCon = undefined

typeRepTyCon :: TypeRep -> TyCon
typeRepTyCon = undefined

typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs = undefined


class Typeable a where
  typeOf :: a -> TypeRep

class Typeable1 f where
  typeOf1 :: f a -> TypeRep

#endif


class Typeable0' f where
  typeOf0' :: f a -> TypeRep

instance (Datatype d) => Typeable0' (M1 D d a) where
  typeOf0' x = mkTyConApp (mkTyCon (datatypeName x)) []


typeOf0default :: (Representable0 a rep, Typeable0' rep)
              => rep x -> a -> TypeRep
typeOf0default rep x = typeOf0' (from0 x `asTypeOf` rep)


class Typeable1' f a where
  typeOf1' :: f a -> TypeRep

instance (Typeable a, Datatype d) => Typeable1' (M1 D d f) a where
  typeOf1' x = mkTyConApp (mkTyCon (datatypeName x)) [typeOf (y x)]
    where y :: M1 D d f b -> b
          y _ = undefined


typeOf1default :: (Representable1 f rep, Typeable1' rep a)
              => rep a -> f a -> TypeRep
typeOf1default rep x = typeOf1' (from1 x `asTypeOf` rep)
