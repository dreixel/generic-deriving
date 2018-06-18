{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GADTSpec where

import Control.Exception (Exception)
import Data.Coerce
import Data.Kind
import Data.Type.Equality hiding ((:~:)(..), (:~~:)(..))
import Generics.Deriving.Base
import Generics.Deriving.Eq
import Generics.Deriving.Foldable
import Generics.Deriving.Functor
import Generics.Deriving.Semigroup
import Generics.Deriving.Show
import Generics.Deriving.TH
import Generics.Deriving.Traversable
import Test.Hspec

data Foo :: Type -> Type -> Type where
  MkFoo1 :: Foo Int b
  MkFoo2 :: (Eq a, Show a) => a -> a -> Foo a b
  MkFoo3 :: (a ~ Int, Int ~~ a) => Foo a b
$(deriveAll0And1 ''Foo)

data Wat :: Type -> Type where
  MkWat1 :: a -> b -> c -> Wat a
  MkWat2 :: Eq b => a -> b -> Wat a
$(deriveAll0And1 ''Wat)

-- Bootstrapping our new, fancy representation types
data ExQuant2 (a :: Type) :: forall k. (a ~> (k -> Type)) -> (k -> Type) where
  ExQuant2 :: { unExQuant2 :: WrappedApply f x p } -> ExQuant2 a f p
$(deriveAll0And1 ''ExQuant2)

data ExContext2 :: forall k. Constraint -> (k -> Type) -> (k -> Type) where
  ExContext2 :: c => { unExContext2 :: f p } -> ExContext2 c f p
$(deriveAll0And1 ''ExContext2)
instance (c => GEq        (f p)) => GEq        (ExContext2 c f p)
instance (c => GShow      (f p)) => GShow      (ExContext2 c f p)
instance (c => GSemigroup (f p)) => GSemigroup (ExContext2 c f p)
instance (c => GFunctor     f) => GFunctor     (ExContext2 c f)
instance (c => GFoldable    f) => GFoldable    (ExContext2 c f)
instance (c => GTraversable f) => GTraversable (ExContext2 c f)

-- From Control.Exception
data Handler a =
  forall e. Exception e => Handler (e -> IO a)
$(deriveAll0And1 ''Handler)
instance GFunctor Handler

-- From Data.Type.Coercion
data Coercion a b = Coercible a b => Coercion
$(deriveAll0 ''Coercion)
instance GEq   (Coercion a b)
instance GShow (Coercion a b)

-- From Data.Type.Equality
infix 4 :~:
data a :~: b = (a ~ b) => Refl
$(deriveAll0 ''(:~:))
instance GEq   (a :~: b)
instance GShow (a :~: b)

infix 4 :~~:
data a :~~: b = (a ~~ b) => HRefl
$(deriveAll0 ''(:~~:))
instance GEq   (a :~~: b)
instance GShow (a :~~: b)

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()
