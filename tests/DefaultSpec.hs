-- |
-- Module      : DefaultSpec
-- Description : Ensure that deriving via (Default a) newtype works
-- License     : BSD-3-Clause
--
-- Maintainer  : generics@haskell.org
-- Stability   : experimental
-- Portability : non-portable
--
-- Tests DerivingVia on GHC versions 8.6 and above. There are no tests on
-- versions below.
--
-- The test check a miscellany of properties of the derived type classes.
-- (Testing all the required properties is beyond the scope of this module.)
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

module DefaultSpec where

import Test.Hspec

#if __GLASGOW_HASKELL__ >= 806
import Test.Hspec.QuickCheck

import Data.Semigroup (First(..), Option(..))
import Data.Foldable (sequenceA_)
import Generics.Deriving hiding (universe)
import Generics.Deriving.Default ()
import Generics.Deriving.Foldable (GFoldable(..))
import Generics.Deriving.Semigroup (GSemigroup(..))
#endif

spec :: Spec
spec = do
  describe "DerivingVia Default" $ do

#if __GLASGOW_HASKELL__ >= 806
    it "GEq is commutative for derivingVia (Default MyType)" . sequenceA_ $
      let commutative :: GEq a => a -> a -> Expectation
          commutative x y = x `geq` y `shouldBe` y `geq` x

          universe :: [MyType]
          universe = MyType <$> [False, True]

      in  commutative <$> universe <*> universe

    it "GShow for MyType is like Show for Bool with derivingVia (Default MyType) but prefixed with 'MyType '" $ do
      gshowsPrec 0 (MyType False) "" `shouldBe` "MyType " <> showsPrec 0 False ""
      gshowsPrec 0 (MyType True) "" `shouldBe` "MyType " <> showsPrec 0 True ""

    it "GEq is commutative for parameterized derivingVia (Default (MyType1 Bool))" . sequenceA_ $
      let commutative :: GEq a => a -> a -> Expectation
          commutative x y = x `geq` y `shouldBe` y `geq` x

          universe :: [MyType1 Bool]
          universe = MyType1 <$> [False, True]

      in  commutative <$> universe <*> universe

    it "GShow for MyType1 Bool is like Show for Bool with derivingVia (Default (MyType1 Bool)) but prefixed with 'MyType1 '" $ do
      gshowsPrec 0 (MyType1 False) "" `shouldBe` "MyType1 " <> showsPrec 0 False ""
      gshowsPrec 0 (MyType1 True) "" `shouldBe` "MyType1 " <> showsPrec 0 True ""

    it "GEq is commutative for derivingVia (Default Bool)" . sequenceA_ $
      let commutative :: GEq a => a -> a -> Expectation
          commutative x y = x `geq` y `shouldBe` y `geq` x

          universe :: [TestEq]
          universe = TestEq <$> [False, True]

      in  commutative <$> universe <*> universe

    it "GENum is correct for derivingVia (Default Bool)" $
      genum `shouldBe` [TestEnum False, TestEnum True]

    it "GShow for TestShow is the same as Show for Bool with derivingVia (Default Bool)" $ do
      gshowsPrec 0 (TestShow False) "" `shouldBe` showsPrec 0 False ""
      gshowsPrec 0 (TestShow True) "" `shouldBe` showsPrec 0 True ""

    it "GSemigroup is like First when instantiated with derivingVia (First Bool)" . sequenceA_ $
      let first' :: (Eq a, Show a, GSemigroup a) => a -> a -> Expectation
          first' x y = x `gsappend` y `shouldBe` x

          universe :: [FirstSemigroup]
          universe = FirstSemigroup <$> [False, True]

      in  first' <$> universe <*> universe

    prop "GFoldable with derivingVia (Default1 Option) acts like mconcat with Maybe (First Bool)" $ \(xs :: [Maybe Bool]) ->
      let ys :: [Maybe (First Bool)]
          -- Note that there is no Arbitrary instance for this type
          ys = fmap First <$> xs

          unTestFoldable :: TestFoldable a -> Maybe a
          unTestFoldable (TestFoldable x) = x

      in  gfoldMap unTestFoldable (TestFoldable <$> ys) `shouldBe` mconcat ys

    it "GFunctor for TestFunctor Bool is as Functor for Maybe Bool" . sequenceA_ $
      let universe :: [Maybe Bool]
          universe = [Nothing, Just False, Just True]

          functor_prop :: Maybe Bool -> Expectation
          functor_prop x = gmap not (TestFunctor x) `shouldBe` TestFunctor (not <$> x)

      in  functor_prop <$> universe

#endif
    return ()

#if __GLASGOW_HASKELL__ >= 806

-- These types all implement instances using `DerivingVia`: most via
-- `Default` (one uses `First`).

newtype TestEq = TestEq Bool
  deriving (GEq) via (Default Bool)
newtype TestEnum = TestEnum Bool
  deriving stock (Eq, Show)
  deriving (GEnum) via (Default Bool)
newtype TestShow = TestShow Bool
  deriving (GShow) via (Default Bool)

newtype FirstSemigroup = FirstSemigroup Bool
  deriving stock (Eq, Show)
  deriving (GSemigroup) via (First Bool)

newtype TestFoldable a = TestFoldable (Maybe a)
  deriving (GFoldable) via (Default1 Option)

newtype TestFunctor a = TestFunctor (Maybe a)
  deriving stock (Eq, Show, Functor)
  deriving (GFunctor) via (Default1 Maybe)

newtype TestHigherEq a = TestHigherEq (Maybe a)
  deriving stock (Generic)
  deriving (GEq) via (Default (TestHigherEq a))

-- These types correspond to the hypothetical examples in the module
-- documentation.

data MyType = MyType Bool
  deriving (Generic)
  deriving (GEq) via (Default MyType)

deriving via (Default MyType) instance GShow MyType

data MyType1 a = MyType1 a
  deriving (Generic, Generic1)
  deriving (GEq) via (Default (MyType1 a))
  deriving (GFunctor) via (Default1 MyType1)

deriving via Default (MyType1 a) instance GShow a => GShow (MyType1 a)
deriving via (Default1 MyType1) instance GFoldable MyType1
#endif
