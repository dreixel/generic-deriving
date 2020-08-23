{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module EmptyCaseSpec (main, spec) where

import Generics.Deriving.Extra.TH
import Test.Hspec

data Empty a
$(deriveAll0And1Options defaultOptions{emptyCaseOptions = True}
                        ''Empty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()
