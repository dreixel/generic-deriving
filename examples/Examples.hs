{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DeriveFunctor #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DeriveGeneric #-}
#endif

module Main (
  -- * Run all tests
  main
  ) where

import Prelude hiding (Either(..))
import Generics.Deriving
import Generics.Deriving.TH


--------------------------------------------------------------------------------
-- Temporary tests for TH generation
--------------------------------------------------------------------------------

data (:/:) f a = MyType1Nil
               | MyType1Cons { myType1Rec :: (f :/: a), myType2Rec :: MyType2 }
               | MyType1Cons2 (f :/: a) Int a (f a)
               | (f :/: a) :/: MyType2

#if __GLASGOW_HASKELL__ >= 701
  deriving Generic
#endif

data MyType2 = MyType2 Float ([] :/: Int)

#if __GLASGOW_HASKELL__ < 701

$(deriveAll ''(:/:))
$(deriveAll ''MyType2)

#else

-- deriving instance Generic (f :/: a)
deriving instance Generic MyType2

#endif

--------------------------------------------------------------------------------
-- Example: Haskell's lists and Maybe
--------------------------------------------------------------------------------

hList1, hList2 :: [Int]
hList1 = [1..10]
hList2 = [2,4..]

maybe1 = Nothing
maybe2 = Just (Just 'p')

double :: [Int] -> [Int]
double []     = []
double (x:xs) = x:x:xs

testsStandard = [ gshow hList1
                , gshow (children maybe2)
                , gshow (transform (const "abc") [])
                , gshow (transform double hList1)
                , gshow (geq hList1 hList1)
                , gshow (geq maybe1 maybe2)
                , gshow (take 5 (genum :: [Maybe Int]))
                , gshow (take 15 (genum :: [[Int]]))
                , gshow (range ([0], [1::Int]))
                , gshow (inRange ([0], [3,5::Int]) hList1) ]

--------------------------------------------------------------------------------
-- Example: trees of integers (kind *)
--------------------------------------------------------------------------------

data Tree = Empty | Branch Int Tree Tree

#if __GLASGOW_HASKELL__ >= 701

deriving instance Generic Tree

instance GShow Tree
instance Uniplate Tree
instance GEnum Tree

#else

$(deriveAll ''Tree)

instance GShow    Tree where gshowsPrec = gshowsPrecdefault
instance Uniplate Tree where
  children   = childrendefault
  context    = contextdefault
  descend    = descenddefault
  descendM   = descendMdefault
  transform  = transformdefault
  transformM = transformdefault
instance GEnum    Tree where genum      = genumDefault

#endif

upgradeTree :: Tree -> Tree
upgradeTree Empty          = Branch 0 Empty Empty
upgradeTree (Branch n l r) = Branch (succ n) l r

-- Example usage
tree = Branch 2 Empty (Branch 1 Empty Empty)
testsTree = [ gshow tree 
            , gshow (children tree)
            , gshow (descend (descend (\_ -> Branch 0 Empty Empty)) tree)
            , gshow (context tree [Branch 1 Empty Empty,Empty])
            , gshow (transform upgradeTree tree)
            , gshow (take 10 (genum :: [Tree])) ]

--------------------------------------------------------------------------------
-- Example: lists (kind * -> *)
--------------------------------------------------------------------------------

data List a = Nil | Cons a (List a) 

#if __GLASGOW_HASKELL__ >= 701
deriving instance Generic (List a)
#else

type Rep0List_ a = D1 List_ ((:+:) (C1 Nil_ U1) (C1 Cons_ ((:*:) (Par0 a) (Rec0 (List a)))))
instance Generic (List a) where
  type Rep (List a) = Rep0List_ a
  from Nil        = M1 (L1 (M1 U1))
  from (Cons h t) = M1 (R1 (M1 ((:*:) (K1 h) (K1 t))))
  to (M1 (L1 (M1 U1)))                     = Nil
  to (M1 (R1 (M1 (K1 h :*: K1 t)))) = Cons h t

#endif

#if __GLASGOW_HASKELL__ >= 705
deriving instance Generic1 List
#else

data List_
data Nil_
data Cons_

instance Datatype List_ where
  datatypeName _ = "List"
  moduleName   _ = "Examples"

instance Constructor Nil_  where conName _ = "Nil"
instance Constructor Cons_ where conName _ = "Cons"

type Rep1List_ = D1 List_ ((:+:) (C1 Nil_ U1) (C1 Cons_ ((:*:) Par1 (Rec1 List))))
instance Generic1 List where
  type Rep1 List = Rep1List_
  from1 Nil        = M1 (L1 (M1 U1))
  from1 (Cons h t) = M1 (R1 (M1 (Par1 h :*: Rec1 t)))
  to1 (M1 (L1 (M1 U1)))                         = Nil
  to1 (M1 (R1 (M1 (Par1 h :*: Rec1 t)))) = Cons h t

#endif

#if __GLASGOW_HASKELL__ < 701
-- Instance for generic functions (should be automatically generated)
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
  transformM = transformdefault

#else

instance                 GFunctor  List
instance (GShow a)    => GShow    (List a)
instance (Uniplate a) => Uniplate (List a)

#endif

-- Example usage
list = Cons 'p' (Cons 'q' Nil)
listlist = Cons list (Cons Nil Nil) -- ["pq",""]

testsList = [ gshow (gmap fromEnum list)
            , gshow (gmap gshow listlist)
            , gshow list
            , gshow listlist
            , gshow (children list)
            , gshow (children listlist) ]


--------------------------------------------------------------------------------
-- Example: Nested datatype, record selectors
--------------------------------------------------------------------------------

data Nested a = Leaf | Nested { value :: a, rec :: Nested [a] }
  deriving Functor

#if __GLASGOW_HASKELL__ >= 701
deriving instance Generic (Nested a)
#endif

#if __GLASGOW_HASKELL__ < 705
$(deriveMeta ''Nested)
#endif

#if __GLASGOW_HASKELL__ < 701
$(deriveRepresentable0 ''Nested)
#endif

#if __GLASGOW_HASKELL__ >= 705
deriving instance Generic1 Nested
#else

type RepNested = D1 Nested_ (C1 Nested_Leaf_ U1 :+: C1 Nested_Nested_ (Par1 :*: Nested :.: Rec1 []))
instance Generic1 Nested where
  type Rep1 Nested = RepNested
  from1 Leaf = M1 (L1 (M1 U1))
  from1 (Nested a l) = M1 (R1 (M1 (Par1 a :*: Comp1 (gmap Rec1 l))))
  to1 (M1 (L1 (M1 U1))) = Leaf
  to1 (M1 (R1 (M1 (Par1 a :*: Comp1 l)))) = Nested a (gmap unRec1 l)
#endif

#if __GLASGOW_HASKELL__ < 701
-- Instance for gshow (should be automatically generated)
instance (GShow a) => GShow (Nested a) where
  gshowsPrec = gshowsPrecdefault

instance GFunctor Nested where
  gmap = gmapdefault

#else 

instance (GShow a) => GShow (Nested a)
instance GFunctor Nested

#endif

-- Example usage
nested :: Nested Int
nested = Nested 1 (Nested [2] (Nested [[3],[4,5],[]] Leaf))
--nested = Nested 1 (Nested (Nested 1 Leaf) Leaf)


testsNested = [ gshow nested
              , gshow (gmap gshow nested) ]

--------------------------------------------------------------------------------
-- Example: Type composition
--------------------------------------------------------------------------------

data Rose a = Rose [a] [Rose a]

#if __GLASGOW_HASKELL__ >= 701
deriving instance Generic (Rose a)
#else

type Rep0Rose a = D1 RoseD (C1 RoseC (Rec0 [a] :*: Rec0 [Rose a]))
instance Generic (Rose a) where
  type Rep (Rose a) = Rep0Rose a
  from (Rose a x) = M1 (M1 (K1 a :*: K1 x))
  to (M1 (M1 (K1 a :*: K1 x))) = Rose a x

#endif

#if __GLASGOW_HASKELL__ >= 705
deriving instance Generic1 Rose
#else

data RoseD
data RoseC

instance Datatype RoseD where
  datatypeName _ = "Rose"
  moduleName   _ = "Examples"

instance Constructor RoseC where conName _ = "Rose"

-- Generic1 instances
type RepRose = D1 RoseD (C1 RoseC (Rec1 [] :*: [] :.: Rec1 Rose))
instance Generic1 Rose where
  type Rep1 Rose = RepRose
  from1 (Rose a x) = M1 (M1 (Rec1 a :*: Comp1 (gmap Rec1 x)))
  to1 (M1 (M1 (Rec1 a :*: Comp1 x))) = Rose a (gmap unRec1 x)

#endif

#if __GLASGOW_HASKELL_ >= 701

instance (GShow a) => GShow (Rose a)
instance GFunctor Rose

#else

-- Instance for gshow (should be automatically generated)
instance (GShow a) => GShow (Rose a) where
  gshowsPrec = gshowsPrecdefault

instance GFunctor Rose where
  gmap = gmapdefault

#endif

-- Example usage
rose1 :: Rose Int
rose1 = Rose [1,2] [Rose [3,4] [], Rose [5] []]

testsRose = [ gshow rose1
            , gshow (gmap gshow rose1) ]

--------------------------------------------------------------------------------
-- Example: Higher-order kinded datatype, type composition
--------------------------------------------------------------------------------

data GRose f a = GRose (f a) (f (GRose f a))

deriving instance (Functor f) => Functor (GRose f)

#if __GLASGOW_HASKELL__ >= 701
deriving instance Generic (GRose f a)
#endif

#if __GLASGOW_HASKELL__ < 705
$(deriveMeta ''GRose)
#endif

#if __GLASGOW_HASKELL__ < 701
$(deriveRepresentable0 ''GRose)
#endif

#if __GLASGOW_HASKELL__ >= 705
deriving instance (Functor f) => Generic1 (GRose f)
#else

type Rep1GRose f = D1 GRose_ (C1 GRose_GRose_ (Rec1 f :*: f :.: (Rec1 (GRose f))))
instance (GFunctor f) => Generic1 (GRose f) where
  type Rep1 (GRose f) = Rep1GRose f
  from1 (GRose a x) = M1 (M1 (Rec1 a :*: Comp1 (gmap Rec1 x)))
  to1 (M1 (M1 (Rec1 a :*: Comp1 x))) = GRose a (gmap unRec1 x)
#endif

#if __GLASGOW_HASKELL__ < 701
-- Requires UndecidableInstances
instance (GShow (f a), GShow (f (GRose f a))) => GShow (GRose f a) where
  gshowsPrec = gshowsPrecdefault

instance (GFunctor f) => GFunctor (GRose f) where
  gmap = gmapdefault

#else

instance (GShow (f a), GShow (f (GRose f a))) => GShow (GRose f a)
instance (Functor f, GFunctor f) => GFunctor (GRose f)

#endif

-- Example usage
grose1 :: GRose [] Int
grose1 = GRose [1,2] [GRose [3] [], GRose [] []]

testsGRose = [ gshow grose1
             , gshow (gmap gshow grose1) ]

--------------------------------------------------------------------------------
-- Example: NGRose (minimal)
--------------------------------------------------------------------------------

-- Cannot represent because of nesting on an argument other than the parameter
{-
data NGRose f a = NGNode a (f (NGRose (Comp f f) a))
data Comp f g a = Comp (f (g a))

type Rep0NGRose f a = Par0 a :*: Rec0 (f (NGRose (Comp f f) a))
instance Generic (NGRose f a) (Rep0NGRose f a) where
  from (NGNode a x) = K1 a :*: K1 x
  to (K1 a :*: K1 x) = NGNode a x

type Rep0Comp f g a = Rec0 (f (g a))
instance Generic (Comp f g a) (Rep0Comp f g a) where
  from (Comp x) = K1 x
  to (K1 x) = Comp x

type Rep1Comp f g = f :.: Rec1 g
instance (GFunctor f) => Generic1 (Comp f g) (Rep1Comp f g) where
  from1 (Comp x) = Comp1 (gmap Rec1 x)
  to1 (Comp1 x) = Comp (gmap unRec1 x)

type Rep1NGRose f = Par1 :*: f :.: Rec1 (NGRose (Comp f f))
instance (GFunctor f) => Generic1 (NGRose f) (Rep1NGRose f) where
  from1 (NGNode a x) = Par1 a :*: (Comp1 (gmap Rec1 x))
  to1 (Par1 a :*: Comp1 x) = NGNode a (gmap unRec1 x)

instance (GShow a, GShow (f (NGRose (Comp f f) a))) => GShow (NGRose f a) where
  gshowsPrec = t undefined where
    t :: (GShow a, GShow (f (NGRose (Comp f f) a))) => Rep0NGRose f a x -> NGRose f a -> ShowS
    t = gshowsPrecdefault

instance (GShow a) => GShow (Comp f g a) where
  gshowsPrec = t undefined where
    t :: (GShow a) => Rep0Comp f g a x -> Comp f g a -> ShowS
    t = gshowsPrecdefault

instance (GFunctor f, GFunctor (Comp f f)) => GFunctor (NGRose f) where
  gmap = t undefined where
    t :: (GFunctor f, GFunctor (Comp f f)) => Rep1NGRose f a -> (a -> b) -> NGRose f a -> NGRose f b
    t = gmapdefault

ngrose1 :: NGRose [] Int
ngrose1 = NGNode 0 [ngrose2, ngrose2]

ngrose2 :: NGRose (Comp [] []) Int
ngrose2 = NGNode 1 (Comp [])

testsNGRose = [ gshow ngrose1
              , gshow (gmap gshow ngrose1) ]
-}

--------------------------------------------------------------------------------
-- Example: Double type composition (minimal)
--------------------------------------------------------------------------------

-- Add this to EHC
unComp (Comp1 x) = x

data Weird a = Weird [[[a]]] deriving Show

type Rep1Weird = [] :.: [] :.: Rec1 []
instance Generic1 Weird where
  type Rep1 Weird = Rep1Weird
  from1 (Weird x) = Comp1 (gmap (Comp1 . gmap Rec1) x)
  to1 (Comp1 x) = Weird (gmap (gmap unRec1 . unComp) x)

#if __GLASGOW_HASKELL__ >= 701

instance GFunctor Weird

#else 

instance GFunctor Weird where
  gmap = gmapdefault

#endif

--------------------------------------------------------------------------------
-- Example: Nested datatype Bush (minimal)
--------------------------------------------------------------------------------

data Bush a = BushNil | BushCons a (Bush (Bush a)) deriving Functor

#if __GLASGOW_HASKELL__ >= 701
deriving instance Generic (Bush a)
#endif

#if __GLASGOW_HASKELL__ < 705
$(deriveMeta ''Bush)
#endif

#if __GLASGOW_HASKELL__ < 701
$(deriveRepresentable0 ''Bush)
#endif

#if __GLASGOW_HASKELL__ >= 705
deriving instance Generic1 Bush
#else

type Rep1Bush = U1 :+: Par1 :*: Bush :.: Rec1 Bush
instance Generic1 Bush where
  type Rep1 Bush = Rep1Bush
  from1 BushNil = L1 U1
  from1 (BushCons a b) = R1 (Par1 a :*: Comp1 (gmap Rec1 b))
  to1 (L1 U1) = BushNil
  to1 (R1 (Par1 a :*: Comp1 b)) = BushCons a (gmap unRec1 b)

#endif

#if __GLASGOW_HASKELL__ < 701

instance GFunctor Bush where
  gmap = gmapdefault

instance (GShow a) => GShow (Bush a) where
  gshowsPrec = gshowsPrecdefault

#else

instance GFunctor Bush
instance (GShow a) => GShow (Bush a)

#endif

-- Example usage
bush1 :: Bush Int
bush1 = BushCons 0 (BushCons (BushCons 1 BushNil) BushNil)

testsBush = [ gshow bush1
            , gshow (gmap gshow bush1) ]

--------------------------------------------------------------------------------
-- Example: Two parameters, datatype constraint, nested on other parameter
--------------------------------------------------------------------------------

-- Any constraints on |b| mean we cannot generate the Generic1 instance
-- Constraints on |a| are just propagated to Generic and generic
-- function instances
data (Show a) => Either a b = Left (Either [a] b) | Right b


-- Generic1 instances
type Rep0Either a b = Rec0 (Either [a] b) :+: Rec0 b
instance (Show a) => Generic (Either a b) where
  type Rep (Either a b) = Rep0Either a b
  from (Left a)  = L1 (K1 a)
  from (Right a) = R1 (K1 a)
  to (L1 (K1 a)) = Left a
  to (R1 (K1 a)) = Right a

type RepEither a = Rec1 (Either [a]) :+: Par1
instance (Show a) => Generic1 (Either a) where
  type Rep1 (Either a) = RepEither a
  from1 (Left a)  = L1 (Rec1 a)
  from1 (Right a) = R1 (Par1 a)
  to1 (L1 (Rec1 a)) = Left a
  to1 (R1 (Par1 a)) = Right a


#if __GLASGOW_HASKELL__ < 701
-- Instance for gshow (should be automatically generated)
instance (Show a, GShow a, GShow b) => GShow (Either a b) where
  gshowsPrec = gshowsPrecdefault

instance (Show a) => GFunctor (Either a) where
  gmap = gmapdefault

#else

instance (Show a, GShow a, GShow b) => GShow (Either a b)
instance (Show a) => GFunctor (Either a)

#endif

either1 :: Either Int Char
either1 = Left either2

either2 :: Either [Int] Char
either2 = Right 'p'

testsEither = [ gshow either1
              , gshow (gmap gshow either1) ]

--------------------------------------------------------------------------------
-- Main tests
--------------------------------------------------------------------------------

main :: IO ()
main = do
        let p = putStrLn . ((++) "- ") . show
        putStrLn "[] and Maybe tests:"
        mapM_ p testsStandard
        putStrLn "Tests for Tree:"
        mapM_ p testsTree
        putStrLn "\nTests for List:"
        mapM_ p testsList
        putStrLn "\nTests for Rose:"
        mapM_ p testsRose
        putStrLn "\nTests for GRose:"
        mapM_ p testsGRose
        putStrLn "\nTests for Either:"
        mapM_ p testsEither
        putStrLn "\nTests for Nested:"
        mapM_ p testsNested
        putStrLn "\nTests for Bush:"
        mapM_ p testsBush
