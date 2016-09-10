{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeInType #-}
#endif

module TypeInTypeSpec (main, spec) where

import Test.Hspec

#if __GLASGOW_HASKELL__ >= 800
import Data.Proxy (Proxy)
import Generics.Deriving.TH

data TyCon x (a :: x) (b :: k) = TyCon k x (Proxy a) (TyCon x a b)
$(deriveAll0And1 ''TyCon)

data family TyFam x (a :: x) (b :: k)
data instance TyFam x (a :: x) (b :: k) = TyFam k x (Proxy a) (TyFam x a b)
$(deriveAll0And1 'TyFam)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()
