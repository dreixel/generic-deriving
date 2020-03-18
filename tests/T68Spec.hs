{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
#endif

module T68Spec (main, spec) where

import Generics.Deriving.TH
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()

type family F68 :: * -> *
type instance F68 = Maybe
data T68 a = MkT68 (F68 a)
$(deriveAll1 ''T68)
