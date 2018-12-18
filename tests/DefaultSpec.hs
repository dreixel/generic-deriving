-- |
-- Module      : DefaultSpec
-- Description : Ensure that deriving via (Default  a)newtype works
-- License     : BSD-3-Clause
--
-- Maintainer  : generics@haskell.org
-- Stability   : experimental
-- Portability : non-portable
--
-- Tests DerivingVia on GHC versions 8.6 and above.
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DerivingVia #-}

module DefaultSpec (main, spec) where

import Test.Hspec
import Generics.Deriving.Default
import Generics.Deriving

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DerivingVia Default" $ do
    undefined

newtype TestEq = TestEq Bool
  deriving (Eq) via (Default Bool)
newtype TestEnum = TestEnum Bool
  deriving (Enum) via (Default Bool)
newtype TestShow = TestShow Bool
  deriving (Show) via (Default Bool)

newtype TestFoldable a = TestFoldable (Maybe a)
  deriving (Foldable) via (Default1 Maybe)
newtype TestFunctor a = TestFunctor (Maybe a)
  deriving (Functor) via (Default1 Maybe)
