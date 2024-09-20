{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module T80Spec (main, spec) where

import Generics.Deriving.TH
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()

newtype T f a b = MkT (f a b)
$(deriveAll1 ''T)
