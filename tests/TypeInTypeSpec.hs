{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module TypeInTypeSpec (main, spec) where

import Data.Proxy (Proxy(..))
import GHC.Generics (Generic1(..))
import Generics.Deriving.Extra.TH
import Test.Hspec

data TyCon x (a :: x) (b :: k) = TyCon k x (Proxy a) (TyCon x a b)
$(deriveAll0And1 ''TyCon)

data family TyFam x (a :: x) (b :: k)
data instance TyFam x (a :: x) (b :: k) = TyFam k x (Proxy a) (TyFam x a b)
$(deriveAll0And1 'TyFam)

gen1PolyKinds :: Generic1 f => f 'True -> Rep1 f 'True
gen1PolyKinds = from1

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
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
