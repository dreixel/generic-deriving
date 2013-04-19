> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE DeriveFunctor #-}

> import Generics.Deriving
> import Generics.Deriving.Monoid

> instance GFunctor UserTree

> --instance GComo UserTree

> data UserTree a = Node a (UserTree a) (UserTree a) | Leaf a | B deriving (Show, Generic, Generic1, Functor)

> cojoiney (Node a l r) = Node (Node a l r) (cojoiney l) (cojoiney r)
> cojoiney (Leaf x) = Leaf (Leaf x)
> cojoiney B = B

> data FTree = F1 Int | F2 Int deriving Generic

 instance GMonoid FTree


> data Foo = Foo Int deriving (Show, Generic)

 instance Annotate (UserTree a)

 instance Annotate Foo

Node (Node 1 (Leaf 2) (Node 3 (Leaf 4) (Leaf 5))) 
   (Leaf (Node 1 (Leaf 2) (Node 3 (Leaf 4) (Leaf 5)))) (Node (Node 1 (Leaf 2) (Node 3 (Leaf 4) (Leaf 5))) (Leaf (Node 1 (Leaf 2) (Node 3 (Leaf 4) (Leaf 5)))) (Leaf (Node 1 (Leaf 2) (Node 3 (Leaf 4) (Leaf 5)))))


Node (Node 1 (Leaf 2) (Leaf 3)) 
   (Leaf (Node 1 (Leaf 2) (Leaf 3))) 
   (Leaf (Node 1 (Leaf 2) (Leaf 3)))


Main.D1UserTree
                          (M1 C
                             Main.C1_0UserTree
                             (M1 S NoSelector Par1
                              :*: (M1 S NoSelector (Rec1 UserTree)
                                   :*: M1 S NoSelector (Rec1 UserTree)))
                           :+: M1 C Main.C1_1UserTree (S1 NoSelector Par1)))
