-- |
-- Module      : DefaultSpec
-- Description : Ensure that deriving via Default newtype works
--
-- Maintainer  : David Baynard <haskell@baynard.me>
-- Stability   : experimental
-- Portability : portable
--
-- Tests DerivingVia on GHC versions 8.6 and above.

module DefaultSpec (main, spec) where

import Test.Hspec
import Generics.Deriving.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DerivingVia Default" $ do
    undefined
