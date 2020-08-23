{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module T68Spec (main, spec) where

import Generics.Deriving.Extra.TH
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()

type family F68 :: * -> *
type instance F68 = Maybe
data T68 a = MkT68 (F68 a)
$(deriveAll1 ''T68)
