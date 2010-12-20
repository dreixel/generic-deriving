{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}

module Generics.Deriving.Instances (
  -- * Representations for base types
    Rep0Char, Rep0Int, Rep0Float
  , Rep0Maybe, Rep1Maybe
  , Rep0List, Rep1List
  ) where

import Generics.Deriving.Base

--------------------------------------------------------------------------------
-- Representation for base types
--------------------------------------------------------------------------------

-- Representation types
{-
type Rep1Par1 = Par1
instance Representable1 Par1 Rep1Par1 where
  from1 = id
  to1 = id

type Rep1Rec1 f = Rec1 f
instance Representable1 (Rec1 f) (Rep1Rec1 f) where
  from1 = id
  to1 = id
-}
-- Kind *

type Rep0Char = Rec0 Char
instance Representable0 Char Rep0Char where
  from0 = K1
  to0 = unK1

type Rep0Int = Rec0 Int
instance Representable0 Int Rep0Int where
  from0 = K1
  to0 = unK1

type Rep0Float = Rec0 Float
instance Representable0 Float Rep0Float where
  from0 = K1
  to0 = unK1

-- etc...

-- Kind * -> *

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

type Rep0Maybe a = D1 Maybe_ (C1 Nothing_ U1 :+: C1 Just_ (Par0 a))
instance Representable0 (Maybe a) (Rep0Maybe a) where
  from0 Nothing  = M1 (L1 (M1 U1))
  from0 (Just x) = M1 (R1 (M1 (K1 x)))
  to0 (M1 (L1 (M1 U1)))     = Nothing
  to0 (M1 (R1 (M1 (K1 x)))) = Just x

type Rep1Maybe = D1 Maybe_ (C1 Nothing_ U1 :+: C1 Just_ Par1)
instance Representable1 Maybe Rep1Maybe where
  from1 Nothing  = M1 (L1 (M1 U1))
  from1 (Just x) = M1 (R1 (M1 (Par1 x)))
  to1 (M1 (L1 (M1 U1)))       = Nothing
  to1 (M1 (R1 (M1 (Par1 x)))) = Just x


data List__
data Nil__
data Cons__

instance Datatype [a] where
  datatypeName _ = "[]"
  moduleName   _ = "Data.List"

instance Constructor Nil__  where conName _ = "[]"
instance Constructor Cons__ where
  conName   _ = ":"
  conFixity _ = Infix RightAssociative 5

type Rep0List a = D1 List__ ((C1 Nil__ U1) :+: (C1 Cons__ (Par0 a :*: Rec0 [a])))
instance Representable0 [a] (Rep0List a) where
  from0 []    = M1 (L1 (M1 U1))
  from0 (h:t) = M1 (R1 (M1 (K1 h :*: K1 t)))
  to0 (M1 (L1 (M1 U1)))              = []
  to0 (M1 (R1 (M1 (K1 h :*: K1 t)))) = h : t

type Rep1List = D1 List__ ((C1 Nil__ U1) :+: (C1 Cons__ (Par1 :*: Rec1 [])))
instance Representable1 [] Rep1List where
  from1 []    = M1 (L1 (M1 U1))
  from1 (h:t) = M1 (R1 (M1 (Par1 h :*: Rec1 t)))
  to1 (M1 (L1 (M1 U1)))                  = []
  to1 (M1 (R1 (M1 (Par1 h :*: Rec1 t)))) = h : t

-- etc...
