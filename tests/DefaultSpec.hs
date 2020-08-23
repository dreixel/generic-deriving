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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DefaultSpec where

import Data.Foldable (sequenceA_)
import Data.Kind
import Data.Semigroup (First(..), Option(..))

import Generics.Deriving.Extra hiding (universe)
import Generics.Deriving.Extra.Default ()
import Generics.Deriving.Extra.Foldable (GFoldable(..))
import Generics.Deriving.Extra.Semigroup (GSemigroup(..))
import Generics.Deriving.Extra.TH (deriveAll0, deriveAll1)
import Generics.Deriving.Extra.Traversable (GTraversable(..), gtraversedefault)

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "DerivingVia Default" $ do

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

-- GADT tests

data GADT1 :: Constraint -> Type -> Type where
  MkGADT1 :: c => a -> GADT1 c a

$(deriveAll0 ''GADT1)

deriving via Default (GADT1 c a) instance (c => GEq        a) => GEq        (GADT1 c a)
deriving via Default (GADT1 c a) instance (c => GSemigroup a) => GSemigroup (GADT1 c a)
deriving via Default (GADT1 c a) instance (c => GShow      a) => GShow      (GADT1 c a)

data GADT2 :: Constraint -> (Type -> Type) -> Type -> Type where
  MkGADT2 :: c => f a -> GADT2 c f a

$(deriveAll1 ''GADT2)

deriving via Default1 (GADT2 c f) instance (c => GCopoint  f) => GCopoint  (GADT2 c f)
deriving via Default1 (GADT2 c f) instance (c => GFoldable f) => GFoldable (GADT2 c f)
deriving via Default1 (GADT2 c f) instance (c => GFunctor  f) => GFunctor  (GADT2 c f)

-- Curse you, roles
-- deriving via Default1 (GADT2 c f) instance (c => GTraversable f) => GTraversable (GADT2 c f)
instance (c => GTraversable f) => GTraversable (GADT2 c f) where
  gtraverse = gtraversedefault
