{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
# if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
# endif
#endif

module TypeInTypeSpec (main, spec) where

import Test.Hspec

#if __GLASGOW_HASKELL__ >= 800
import Data.Proxy (Proxy(..))
import Generics.Deriving.TH

# if MIN_VERSION_base(4,10,0)
import Generics.Deriving (Generic1(..))
# endif

data TyCon x (a :: x) (b :: k) = TyCon k x (Proxy a) (TyCon x a b)
$(deriveAll0And1 ''TyCon)

data family TyFam x (a :: x) (b :: k)
data instance TyFam x (a :: x) (b :: k) = TyFam k x (Proxy a) (TyFam x a b)
$(deriveAll0And1 'TyFam)

# if MIN_VERSION_base(4,10,0)
gen1PolyKinds :: Generic1 f => f 'True -> Rep1 f 'True
gen1PolyKinds = from1
# endif
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_base(4,10,0)
    describe "TyCon Bool 'False 'True" $
      it "has an appropriately kinded Generic1 instance" $
        let rep :: Rep1 (TyCon Bool 'False) 'True
            rep = gen1PolyKinds $ let x = TyCon True False Proxy x in x
         in seq rep () `shouldBe` ()
    describe "TyFam Bool 'False 'True" $
      it "has an appropriately kinded Generic1 instance" $
        let rep :: Rep1 (TyFam Bool 'False) 'True
            rep = gen1PolyKinds $ let x = TyFam True False Proxy x in x
         in seq rep () `shouldBe` ()
#else
    return ()
#endif
