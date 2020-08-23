{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ExampleSpec (main, spec) where

import           Generics.Deriving.Extra
import           Generics.Deriving.Extra.TH

import           GHC.Exts (Addr#, Char#, Double#, Float#, Int#, Word#)

import           Prelude hiding (Either(..))

import           Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)

import qualified Text.Read.Lex (Lexeme)

-------------------------------------------------------------------------------
-- Example: Haskell's lists and Maybe
-------------------------------------------------------------------------------

hList:: [Int]
hList = [1..10]

maybe1, maybe2 :: Maybe (Maybe Char)
maybe1 = Nothing
maybe2 = Just (Just 'p')

double :: [Int] -> [Int]
double []     = []
double (x:xs) = x:x:xs

-------------------------------------------------------------------------------
-- Example: trees of integers (kind *)
-------------------------------------------------------------------------------

data Tree = Empty | Branch Int Tree Tree

$(deriveAll0 ''Tree)

instance GShow Tree where
    gshowsPrec = gshowsPrecdefault

instance Uniplate Tree where
  children   = childrendefault
  context    = contextdefault
  descend    = descenddefault
  descendM   = descendMdefault
  transform  = transformdefault
  transformM = transformMdefault

instance GEnum Tree where
    genum = genumDefault

upgradeTree :: Tree -> Tree
upgradeTree Empty          = Branch 0 Empty Empty
upgradeTree (Branch n l r) = Branch (succ n) l r

tree :: Tree
tree = Branch 2 Empty (Branch 1 Empty Empty)

-------------------------------------------------------------------------------
-- Example: lists (kind * -> *)
-------------------------------------------------------------------------------

data List a = Nil | Cons a (List a)

$(deriveAll0And1 ''List)

instance GFunctor List where
  gmap = gmapdefault

instance (GShow a) => GShow (List a) where
  gshowsPrec = gshowsPrecdefault

instance (Uniplate a) => Uniplate (List a) where
  children   = childrendefault
  context    = contextdefault
  descend    = descenddefault
  descendM   = descendMdefault
  transform  = transformdefault
  transformM = transformMdefault

list :: List Char
list = Cons 'p' (Cons 'q' Nil)

listlist :: List (List Char)
listlist = Cons list (Cons Nil Nil) -- ["pq",""]

-------------------------------------------------------------------------------
-- Example: Type composition
-------------------------------------------------------------------------------

data Rose a = Rose [a] [Rose a]

$(deriveAll0And1 ''Rose)

instance (GShow a) => GShow (Rose a) where
  gshowsPrec = gshowsPrecdefault

instance GFunctor Rose where
  gmap = gmapdefault

-- Example usage
rose1 :: Rose Int
rose1 = Rose [1,2] [Rose [3,4] [], Rose [5] []]

-------------------------------------------------------------------------------
-- Example: Higher-order kinded datatype, type composition
-------------------------------------------------------------------------------

data GRose f a = GRose (f a) (f (GRose f a))
deriving instance Functor f => Functor (GRose f)

$(deriveRepresentable0 ''GRose)
$(deriveRep1           ''GRose)
instance Functor f => Generic1 (GRose f) where
  type Rep1 (GRose f) = $(makeRep1 ''GRose) f
  from1 = $(makeFrom1 ''GRose)
  to1   = $(makeTo1 ''GRose)

instance (GShow (f a), GShow (f (GRose f a))) => GShow (GRose f a) where
  gshowsPrec = gshowsPrecdefault

instance (Functor f, GFunctor f) => GFunctor (GRose f) where
  gmap = gmapdefault

grose1 :: GRose [] Int
grose1 = GRose [1,2] [GRose [3] [], GRose [] []]

-------------------------------------------------------------------------------
-- Example: Two parameters, nested on other parameter
-------------------------------------------------------------------------------

data Either a b = Left (Either [a] b) | Right b

$(deriveAll0And1 ''Either)

instance (GShow a, GShow b) => GShow (Either a b) where
  gshowsPrec = gshowsPrecdefault

instance GFunctor (Either a) where
  gmap = gmapdefault

either1 :: Either Int Char
either1 = Left either2

either2 :: Either [Int] Char
either2 = Right 'p'

-------------------------------------------------------------------------------
-- Example: Nested datatype, record selectors
-------------------------------------------------------------------------------

data Nested a = Leaf | Nested { value :: a, rec :: Nested [a] }
  deriving Functor

$(deriveAll0And1 ''Nested)

instance (GShow a) => GShow (Nested a) where
  gshowsPrec = gshowsPrecdefault

instance GFunctor Nested where
  gmap = gmapdefault

nested :: Nested Int
nested = Nested { value = 1, rec = Nested [2] (Nested [[3],[4,5],[]] Leaf) }

-------------------------------------------------------------------------------
-- Example: Nested datatype Bush (minimal)
-------------------------------------------------------------------------------

data Bush a = BushNil | BushCons a (Bush (Bush a)) deriving Functor

$(deriveAll0And1 ''Bush)

instance GFunctor Bush where
  gmap = gmapdefault

instance (GShow a) => GShow (Bush a) where
  gshowsPrec = gshowsPrecdefault

bush1 :: Bush Int
bush1 = BushCons 0 (BushCons (BushCons 1 BushNil) BushNil)

-------------------------------------------------------------------------------
-- Example: Double type composition (minimal)
-------------------------------------------------------------------------------

data Weird a = Weird [[[a]]] deriving Show

$(deriveAll0And1 ''Weird)

instance GFunctor Weird where
  gmap = gmapdefault

--------------------------------------------------------------------------------
-- Temporary tests for TH generation
--------------------------------------------------------------------------------

data Empty a

data (:/:) f a = MyType1Nil
               | MyType1Cons { _myType1Rec :: (f :/: a), _myType2Rec :: MyType2 }
               | MyType1Cons2 (f :/: a) Int a (f a)
               | (f :/: a) :/: MyType2

infixr 5 :!@!:
data GADTSyntax a b where
  GADTPrefix :: d -> c -> GADTSyntax c d
  (:!@!:)    :: e -> f -> GADTSyntax e f

data MyType2 = MyType2 Float ([] :/: Int)
data PlainHash a = Hash a Addr# Char# Double# Float# Int# Word#

-- Test to see if generated names are unique
data Lexeme = Lexeme

data family MyType3
  (a :: v) (b :: w) (c :: x)      (d :: y) (e :: z)
newtype instance MyType3 (f p) (f p) f p q = MyType3Newtype q
data    instance MyType3 Bool  ()    f p q = MyType3True | MyType3False
data    instance MyType3 Int   ()    f p q = MyType3Hash q Addr# Char# Double# Float# Int# Word#

$(deriveAll0And1 ''Empty)
$(deriveAll0And1 ''(:/:))
$(deriveAll0And1 ''GADTSyntax)
$(deriveAll0     ''MyType2)
$(deriveAll0And1 ''PlainHash)
$(deriveAll0     ''ExampleSpec.Lexeme)
$(deriveAll0     ''Text.Read.Lex.Lexeme)

$(deriveAll0And1 'MyType3Newtype)
$(deriveAll0And1 'MyType3False)
$(deriveAll0And1 'MyType3Hash)

-------------------------------------------------------------------------------
-- Unit tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "[] and Maybe tests" $ do
        it "gshow hList" $
            gshow hList `shouldBe`
                "[1,2,3,4,5,6,7,8,9,10]"

        it "gshow (children maybe2)" $
            gshow (children maybe2) `shouldBe`
                "[]"

        it "gshow (transform (const \"abc\") [])" $
            gshow (transform (const "abc") []) `shouldBe`
                "\"abc\""

        it "gshow (transform double hList)" $
            gshow (transform double hList) `shouldBe`
                "[1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10]"

        it "gshow (geq hList hList)" $
            gshow (geq hList hList) `shouldBe`
                "True"

        it "gshow (geq maybe1 maybe2)" $
            gshow (geq maybe1 maybe2) `shouldBe`
                "False"

        it "gshow (take 5 genum)" $
            gshow (take 5 (genum :: [Maybe Int])) `shouldBe`
                "[Nothing,Just 0,Just -1,Just 1,Just -2]"

        it "gshow (take 15 genum)" $
            gshow (take 15 (genum :: [[Int]])) `shouldBe`
                "[[],[0],[0,0],[-1],[0,0,0],[-1,0],[1],[0,-1],[-1,0,0],[1,0],[-2],[0,0,0,0],[-1,-1],[1,0,0],[-2,0]]"

        it "gshow (range ([0], [1]))" $
            gshow (range ([0], [1::Int])) `shouldBe`
                "[[0],[0,0],[-1],[0,0,0],[-1,0]]"

        it "gshow (inRange ([0], [3,5]) hList)" $
            gshow (inRange ([0], [3,5::Int]) hList) `shouldBe`
                "False"

    describe "Tests for Tree" $ do
        it "gshow tree" $
            gshow tree `shouldBe`
                "Branch 2 Empty (Branch 1 Empty Empty)"

        it "gshow (children tree)" $
            gshow (children tree) `shouldBe`
                "[Empty,Branch 1 Empty Empty]"

        it "gshow (descend (descend (\\_ -> Branch 0 Empty Empty)) tree)" $
            gshow (descend (descend (\_ -> Branch 0 Empty Empty)) tree) `shouldBe`
                "Branch 2 Empty (Branch 1 (Branch 0 Empty Empty) (Branch 0 Empty Empty))"

        it "gshow (context tree [Branch 1 Empty Empty,Empty])" $
            gshow (context tree [Branch 1 Empty Empty,Empty]) `shouldBe`
                "Branch 2 (Branch 1 Empty Empty) Empty"

        it "gshow (transform upgradeTree tree)" $
            gshow (transform upgradeTree tree) `shouldBe`
                "Branch 3 (Branch 0 Empty Empty) (Branch 2 (Branch 0 Empty Empty) (Branch 0 Empty Empty))"

        it "gshow (take 10 genum)" $ do
            gshow (take 10 (genum :: [Tree])) `shouldBe`
                "[Empty,Branch 0 Empty Empty,Branch 0 Empty (Branch 0 Empty Empty),Branch -1 Empty Empty,Branch 0 (Branch 0 Empty Empty) Empty,Branch -1 Empty (Branch 0 Empty Empty),Branch 1 Empty Empty,Branch 0 Empty (Branch 0 Empty (Branch 0 Empty Empty)),Branch -1 (Branch 0 Empty Empty) Empty,Branch 1 Empty (Branch 0 Empty Empty)]"

    describe "Tests for List" $ do
        it "gshow (gmap fromEnum list)" $
            gshow (gmap fromEnum list) `shouldBe`
                "Cons 112 (Cons 113 Nil)"

        it "gshow (gmap gshow listlist)" $
            gshow (gmap gshow listlist) `shouldBe`
                "Cons \"Cons 'p' (Cons 'q' Nil)\" (Cons \"Nil\" Nil)"

        it "gshow list" $
            gshow list `shouldBe`
                "Cons 'p' (Cons 'q' Nil)"

        it "gshow listlist" $
            gshow listlist `shouldBe`
                "Cons (Cons 'p' (Cons 'q' Nil)) (Cons Nil Nil)"

        it "gshow (children list)" $
            gshow (children list) `shouldBe`
                "[Cons 'q' Nil]"

        it "gshow (children listlist)" $
            gshow (children listlist) `shouldBe`
                "[Cons Nil Nil]"

    describe "Tests for Rose" $ do
        it "gshow rose1" $
            gshow rose1 `shouldBe`
                "Rose [1,2] [Rose [3,4] [],Rose [5] []]"

        it "gshow (gmap gshow rose1)" $
            gshow (gmap gshow rose1) `shouldBe`
                "Rose [\"1\",\"2\"] [Rose [\"3\",\"4\"] [],Rose [\"5\"] []]"

    describe "Tests for GRose" $ do
        it "gshow grose1" $
            gshow grose1 `shouldBe`
                "GRose [1,2] [GRose [3] [],GRose [] []]"

        it "gshow (gmap gshow grose1)" $
            gshow (gmap gshow grose1) `shouldBe`
                "GRose [\"1\",\"2\"] [GRose [\"3\"] [],GRose [] []]"

    describe "Tests for Either" $ do
        it "gshow either1" $
            gshow either1 `shouldBe`
                "Left Right 'p'"

        it "gshow (gmap gshow either1)" $
            gshow (gmap gshow either1) `shouldBe`
                "Left Right \"'p'\""

    describe "Tests for Nested" $ do
        it "gshow nested" $
            gshow nested `shouldBe`
                "Nested {value = 1, rec = Nested {value = [2], rec = Nested {value = [[3],[4,5],[]], rec = Leaf}}}"

        it "gshow (gmap gshow nested)" $
            gshow (gmap gshow nested) `shouldBe`
                "Nested {value = \"1\", rec = Nested {value = [\"2\"], rec = Nested {value = [[\"3\"],[\"4\",\"5\"],[]], rec = Leaf}}}"

    describe "Tests for Bush" $ do
        it "gshow bush1" $
            gshow bush1 `shouldBe`
                "BushCons 0 (BushCons (BushCons 1 BushNil) BushNil)"

        it "gshow (gmap gshow bush1)" $
            gshow (gmap gshow bush1) `shouldBe`
                "BushCons \"0\" (BushCons (BushCons \"1\" BushNil) BushNil)"
