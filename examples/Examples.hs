{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (

  -- * Run all tests
  main

  ) where


import Prelude hiding (Either(..))
import Generics.Deriving


--------------------------------------------------------------------------------
-- Example: Haskell's lists and Maybe
--------------------------------------------------------------------------------

hList1, hList2 :: [Int]
hList1 = [1..10]
hList2 = [2,4..]

maybe1 = Nothing
maybe2 = Just (Just 'p')

testsStandard = [ gshow hList1
                , gshow (children maybe2)
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

#ifdef __UHC__

deriving instance GShow Tree
deriving instance Uniplate Tree
deriving instance GEnum Tree

#else

data Tree_
data Empty_
data Branch_

instance Datatype Tree_ where
  datatypeName _ = "Tree"
  moduleName   _ = "Examples"

instance Constructor Empty_  where conName _ = "Empty"
instance Constructor Branch_ where conName _ = "Branch"

-- Only a Representable0 instance is needed (no Representable1)
type Rep0Tree = D1 Tree_ (C1 Empty_ U1 :+: 
                          C1 Branch_ (Rec0 Int :*: (Rec0 Tree :*: Rec0 Tree)))
instance Representable0 Tree Rep0Tree where  
  from0 Empty          = M1 (L1 (M1 U1))
  from0 (Branch i l r) = M1 (R1 (M1 (K1 i :*: (K1 l :*: K1 r))))
  to0 (M1 (L1 (M1 U1)))                         = Empty
  to0 (M1 (R1 (M1 (K1 i :*: (K1 l :*: K1 r))))) = Branch i l r

instance GShow Tree where gshows = gshowsdefault (undefined :: Rep0Tree x)
instance Uniplate Tree where children = childrendefault (undefined :: Rep0Tree x)
instance GEnum Tree where genum = genumDefault (undefined :: Rep0Tree x)
instance Typeable Tree where typeOf = typeOf0default (undefined :: Rep0Tree x)

#endif

-- Example usage
tree = Branch 2 Empty (Branch 1 Empty Empty)
testsTree = [ gshow tree 
            , gshow (children tree)
            , gshow (take 10 (genum :: [Tree])) ]

--------------------------------------------------------------------------------
-- Example: lists (kind * -> *)
--------------------------------------------------------------------------------

data List a = Nil | Cons a (List a) 

#ifdef __UHC__

deriving instance (GShow a) => GShow (List a)
deriving instance GFunctor List
deriving instance Uniplate (List a)

#else

data List_
data Nil_
data Cons_

instance Datatype List_ where
  datatypeName _ = "List"
  moduleName   _ = "Examples"

instance Constructor Nil_  where conName _ = "Nil"
instance Constructor Cons_ where conName _ = "Cons"

type Rep0List_ a = D1 List_ ((:+:) (C1 Nil_ U1) (C1 Cons_ ((:*:) (Par0 a) (Rec0 (List a)))))
instance Representable0 (List a) (Rep0List_ a) where
  from0 Nil        = M1 (L1 (M1 U1))
  from0 (Cons h t) = M1 (R1 (M1 ((:*:) (K1 h) (K1 t))))
  to0 (M1 (L1 (M1 U1)))                     = Nil
  to0 (M1 (R1 (M1 (K1 h :*: K1 t)))) = Cons h t

type Rep1List_ = D1 List_ ((:+:) (C1 Nil_ U1) (C1 Cons_ ((:*:) Par1 (Rec1 List))))
instance Representable1 List Rep1List_ where
  from1 Nil        = M1 (L1 (M1 U1))
  from1 (Cons h t) = M1 (R1 (M1 (Par1 h :*: Rec1 t)))
  to1 (M1 (L1 (M1 U1)))                         = Nil
  to1 (M1 (R1 (M1 (Par1 h :*: Rec1 t)))) = Cons h t

-- Instance for generic functions (should be automatically generated)
instance GFunctor List where
  gmap = t undefined where
    t :: Rep1List_ a -> (a -> b) -> List a -> List b
    t = gmapdefault
{-
instance (Typeable a) => Typeable1 List where
  typeOf1 = t undefined where
    t :: (Typeable a) => Rep1List_ a -> List a -> TypeRep
    t = typeOf1default


instance GFoldable List where
  gfoldMap = gfoldMapdefault (undefined :: RepList x)

instance GTraversable List where
  gtraverse = gtraversedefault (undefined :: RepList x)
-}

instance (GShow a) => GShow (List a) where
  gshows = t undefined where
    t :: (GShow a) => Rep0List_ a x -> List a -> ShowS
    t = gshowsdefault

instance (Uniplate a) => Uniplate (List a) where
  children = t undefined where
    t :: (Uniplate a) => Rep0List_ a x -> List a -> [List a]
    t = childrendefault

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

#ifdef __UHC__

deriving instance (GShow a) => GShow (Nested a)
deriving instance GFunctor Nested

#else

data NestedD
data NestedC
data Leaf_
data Value
data Rec

instance Datatype NestedD where
  datatypeName _ = "Nested"
  moduleName   _ = "Examples"

instance Constructor NestedC where
  conName _     = "Nested"
  conIsRecord _ = True

instance Constructor Leaf_ where conName _     = "Leaf"

instance Selector Value where selName _ = "value"
instance Selector Rec   where selName _ = "rec"

-- Representable1 instances
type Rep0Nested a = D1 NestedD (    C1 Leaf_ U1 
                                :+: C1 NestedC (    S1 Value (Par0 a)
                                                :*: S1 Rec (Rec0 (Nested [a]))))
instance Representable0 (Nested a) (Rep0Nested a) where
  from0 Leaf = M1 (L1 (M1 U1))
  from0 (Nested a l) = M1 (R1 (M1 (M1 (K1 a) :*: M1 (K1 l))))
  to0 (M1 (L1 (M1 U1))) = Leaf
  to0 (M1 (R1 (M1 (M1 (K1 a) :*: M1 (K1 l))))) = Nested a l

type RepNested = D1 NestedD (C1 Leaf_ U1 :+: C1 NestedC (Par1 :*: Nested :.: Rec1 []))
instance Representable1 Nested RepNested where
  from1 Leaf = M1 (L1 (M1 U1))
  from1 (Nested a l) = M1 (R1 (M1 (Par1 a :*: Comp1 (gmap Rec1 l))))
  to1 (M1 (L1 (M1 U1))) = Leaf
  to1 (M1 (R1 (M1 (Par1 a :*: Comp1 l)))) = Nested a (gmap unRec1 l)

-- Instance for gshow (should be automatically generated)
instance (GShow a) => GShow (Nested a) where
  gshows = t undefined where
    t :: (GShow a) => Rep0Nested a x -> Nested a -> ShowS
    t = gshowsdefault

instance GFunctor Nested where
  gmap = t undefined where
    t :: RepNested a -> (a -> b) -> Nested a -> Nested b
    t = gmapdefault
{-
instance (GFoldable f) => GFoldable (GRose f) where
  gfoldMap f (x :: GRose f a) = gfoldMapdefault (undefined :: RepGRose f x) f x

instance (GTraversable f) => GTraversable (GRose f) where
  gtraverse f (x :: GRose f a) = gtraversedefault (undefined :: RepGRose f x) f x
-}

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

#ifdef __UHC__

deriving instance (GShow a) => GShow (Rose a)
deriving instance GFunctor Rose

#else

data RoseD
data RoseC

instance Datatype RoseD where
  datatypeName _ = "Rose"
  moduleName   _ = "Examples"

instance Constructor RoseC where conName _ = "Rose"

-- Representable1 instances
type Rep0Rose a = D1 RoseD (C1 RoseC (Rec0 [a] :*: Rec0 [Rose a]))
instance Representable0 (Rose a) (Rep0Rose a) where
  from0 (Rose a x) = M1 (M1 (K1 a :*: K1 x))
  to0 (M1 (M1 (K1 a :*: K1 x))) = Rose a x

type RepRose = D1 RoseD (C1 RoseC (Rec1 [] :*: [] :.: Rec1 Rose))
instance Representable1 Rose RepRose where
  from1 (Rose a x) = M1 (M1 (Rec1 a :*: Comp1 (gmap Rec1 x)))
  to1 (M1 (M1 (Rec1 a :*: Comp1 x))) = Rose a (gmap unRec1 x)

-- Instance for gshow (should be automatically generated)
instance (GShow a) => GShow (Rose a) where
  gshows = t undefined where
    t :: (GShow a) => Rep0Rose a x -> Rose a -> ShowS
    t = gshowsdefault

instance GFunctor Rose where
  gmap = t undefined where
    t :: RepRose a -> (a -> b) -> Rose a -> Rose b
    t = gmapdefault
{-
instance GFoldable Rose where
  gfoldMap = gfoldMapdefault (undefined :: RepRose x)

instance GTraversable Rose where
  gtraverse = gtraversedefault (undefined :: RepRose x)
-}

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

#ifdef __UHC__

deriving instance (GShow (f a), GShow (f (GRose f a))) =>  GShow (GRose f a)
deriving instance (GFunctor f) => GFunctor (GRose f)

#else

data GRoseD
data GRoseC

instance Datatype GRoseD where
  datatypeName _ = "GRose"
  moduleName   _ = "Examples"

instance Constructor GRoseC where conName _ = "GRose"

type Rep0GRose f a = D1 GRoseD (C1 GRoseC (Rec0 (f a) :*: Rec0 (f (GRose f a))))
instance Representable0 (GRose f a) (Rep0GRose f a) where
  from0 (GRose a x) = M1 (M1 (K1 a :*: K1 x))
  to0 (M1 (M1 (K1 a :*: K1 x))) = GRose a x

type Rep1GRose f = D1 GRoseD (C1 GRoseC (Rec1 f :*: f :.: (Rec1 (GRose f))))
instance (GFunctor f) => Representable1 (GRose f) (Rep1GRose f) where
  from1 (GRose a x) = M1 (M1 (Rec1 a :*: Comp1 (gmap Rec1 x)))
  to1 (M1 (M1 (Rec1 a :*: Comp1 x))) = GRose a (gmap unRec1 x)

-- Requires UndecidableInstances
instance (GShow (f a), GShow (f (GRose f a))) => GShow (GRose f a) where
  gshows = t undefined where
    t :: (GShow (f a), GShow (f (GRose f a))) => Rep0GRose f a x -> GRose f a -> ShowS
    t = gshowsdefault

instance (GFunctor f) => GFunctor (GRose f) where
  gmap = t undefined where
    t :: (GFunctor f) => Rep1GRose f a -> (a -> b) -> GRose f a -> GRose f b
    t = gmapdefault
{-
instance (GFoldable f) => GFoldable (GRose f) where
  gfoldMap f (x :: GRose f a) = gfoldMapdefault (undefined :: RepGRose f x) f x

instance (GTraversable f) => GTraversable (GRose f) where
  gtraverse f (x :: GRose f a) = gtraversedefault (undefined :: RepGRose f x) f x
-}

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
instance Representable0 (NGRose f a) (Rep0NGRose f a) where
  from0 (NGNode a x) = K1 a :*: K1 x
  to0 (K1 a :*: K1 x) = NGNode a x

type Rep0Comp f g a = Rec0 (f (g a))
instance Representable0 (Comp f g a) (Rep0Comp f g a) where
  from0 (Comp x) = K1 x
  to0 (K1 x) = Comp x

type Rep1Comp f g = f :.: Rec1 g
instance (GFunctor f) => Representable1 (Comp f g) (Rep1Comp f g) where
  from1 (Comp x) = Comp1 (gmap Rec1 x)
  to1 (Comp1 x) = Comp (gmap unRec1 x)

type Rep1NGRose f = Par1 :*: f :.: Rec1 (NGRose (Comp f f))
instance (GFunctor f) => Representable1 (NGRose f) (Rep1NGRose f) where
  from1 (NGNode a x) = Par1 a :*: (Comp1 (gmap Rec1 x))
  to1 (Par1 a :*: Comp1 x) = NGNode a (gmap unRec1 x)

instance (GShow a, GShow (f (NGRose (Comp f f) a))) => GShow (NGRose f a) where
  gshows = t undefined where
    t :: (GShow a, GShow (f (NGRose (Comp f f) a))) => Rep0NGRose f a x -> NGRose f a -> ShowS
    t = gshowsdefault

instance (GShow a) => GShow (Comp f g a) where
  gshows = t undefined where
    t :: (GShow a) => Rep0Comp f g a x -> Comp f g a -> ShowS
    t = gshowsdefault

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
instance Representable1 Weird Rep1Weird where
  from1 (Weird x) = Comp1 (gmap (Comp1 . gmap Rec1) x)
  to1 (Comp1 x) = Weird (gmap (gmap unRec1 . unComp) x)


instance GFunctor Weird where
  gmap = t undefined where
    t :: Rep1Weird a -> (a -> b) -> Weird a -> Weird b
    t = gmapdefault


--------------------------------------------------------------------------------
-- Example: Two parameters, datatype constraint, nested on other parameter
--------------------------------------------------------------------------------

-- Any constraints on |b| mean we cannot generate the Representable1 instance
-- Constraints on |a| are just propagated to Representable0 and generic
-- function instances
data (Show a) => Either a b = Left (Either [a] b) | Right b

-- Representable1 instances
type Rep0Either a b = Rec0 (Either [a] b) :+: Rec0 b
instance (Show a) => Representable0 (Either a b) (Rep0Either a b) where
  from0 (Left a)  = L1 (K1 a)
  from0 (Right a) = R1 (K1 a)
  to0 (L1 (K1 a)) = Left a
  to0 (R1 (K1 a)) = Right a

type RepEither a = Rec1 (Either [a]) :+: Par1
instance (Show a) => Representable1 (Either a) (RepEither a) where
  from1 (Left a)  = L1 (Rec1 a)
  from1 (Right a) = R1 (Par1 a)
  to1 (L1 (Rec1 a)) = Left a
  to1 (R1 (Par1 a)) = Right a

-- Instance for gshow (should be automatically generated)
instance (Show a, GShow a, GShow b) => GShow (Either a b) where
  gshows = t undefined where
    t :: (Show a, GShow a, GShow b) => Rep0Either a b x -> Either a b -> ShowS
    t = gshowsdefault

instance (Show a) => GFunctor (Either a) where
  gmap = t undefined where
    t :: (Show a) => RepEither a b -> (b -> c) -> Either a b -> Either a c
    t = gmapdefault

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
