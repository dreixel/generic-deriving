{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

module T80Spec (main, spec) where

import Generics.Deriving.TH
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()

newtype T f a b = MkT (f a b)
$(deriveAll1 ''T)
