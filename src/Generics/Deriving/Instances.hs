{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generics.Deriving.Instances (
-- Only instances from Generics.Deriving.Base
-- and the Generic1 instances
    Rep1Maybe, Rep1List
#if __GLASGOW_HASKELL__ < 701
  -- * Representations for base types
  , Rep0Char, Rep0Int, Rep0Float
  , Rep0Maybe, Rep0List
#endif
  ) where

import Generics.Deriving.Base

#if __GLASGOW_HASKELL__ < 701
--------------------------------------------------------------------------------
-- Representation for base types
--------------------------------------------------------------------------------

-- Representation types
{-
type Rep1Par1 = Par1
instance Generic1 Par1 Rep1Par1 where
  from1 = id
  to1 = id

type Rep1Rec1 f = Rec1 f
instance Generic1 (Rec1 f) (Rep1Rec1 f) where
  from1 = id
  to1 = id
-}
-- Kind *

type Rep0Char = Rec0 Char
instance Generic Char where
  type Rep Char = Rep0Char
  from = K1
  to = unK1

type Rep0Int = Rec0 Int
instance Generic Int where
  type Rep Int = Rep0Int
  from = K1
  to = unK1

type Rep0Float = Rec0 Float
instance Generic Float where
  type Rep Float = Rep0Float
  from = K1
  to = unK1

-- etc...

-- Kind * -> *

type Rep0Maybe a = D1 Maybe_ (C1 Nothing_ U1 :+: C1 Just_ (Par0 a))
instance Generic (Maybe a) where
  type Rep (Maybe a) = Rep0Maybe a
  from Nothing  = M1 (L1 (M1 U1))
  from (Just x) = M1 (R1 (M1 (K1 x)))
  to (M1 (L1 (M1 U1)))     = Nothing
  to (M1 (R1 (M1 (K1 x)))) = Just x

type Rep0List a = D1 List__ ((C1 Nil__ U1) :+: (C1 Cons__ (Par0 a :*: Rec0 [a])))
instance Generic [a] where
  type Rep [a] = Rep0List a
  from []    = M1 (L1 (M1 U1))
  from (h:t) = M1 (R1 (M1 (K1 h :*: K1 t)))
  to (M1 (L1 (M1 U1)))              = []
  to (M1 (R1 (M1 (K1 h :*: K1 t)))) = h : t
#endif

-- GHC 7.2 still needs these instances

data Maybe_
data Nothing_
data Just_

instance Datatype Maybe_ where
  datatypeName _ = "Maybe"
  moduleName   _ = "Representation"

instance Constructor Nothing_ where
  conName _ = "Nothing"

instance Constructor Just_ where
  conName _ = "Just"


type Rep1Maybe = D1 Maybe_ (C1 Nothing_ U1 :+: C1 Just_ Par1)
instance Generic1 Maybe where
  type Rep1 Maybe = Rep1Maybe
  from1 Nothing  = M1 (L1 (M1 U1))
  from1 (Just x) = M1 (R1 (M1 (Par1 x)))
  to1 (M1 (L1 (M1 U1)))       = Nothing
  to1 (M1 (R1 (M1 (Par1 x)))) = Just x


data List__
data Nil__
data Cons__

instance Datatype List__ where
  datatypeName _ = "[]"
  moduleName   _ = "Data.List"

instance Constructor Nil__  where conName _ = "[]"
instance Constructor Cons__ where
  conName   _ = ":"
  conFixity _ = Infix RightAssociative 5


type Rep1List = D1 List__ ((C1 Nil__ U1) :+: (C1 Cons__ (Par1 :*: Rec1 [])))
instance Generic1 [] where
  type Rep1 [] = Rep1List
  from1 []    = M1 (L1 (M1 U1))
  from1 (h:t) = M1 (R1 (M1 (Par1 h :*: Rec1 t)))
  to1 (M1 (L1 (M1 U1)))                  = []
  to1 (M1 (R1 (M1 (Par1 h :*: Rec1 t)))) = h : t

-- etc...
