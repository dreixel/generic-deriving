{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generics.Deriving.Instances (
-- Only instances from Generics.Deriving.Base
-- and the Generic1 instances
#if __GLASGOW_HASKELL__ < 705
    Rep1Maybe, Rep1List
#endif
#if __GLASGOW_HASKELL__ < 701
  -- * Representations for base types
  , Rep0Char, Rep0Int, Rep0Float
  , Rep0Maybe, Rep0List
#endif
  ) where

#if __GLASGOW_HASKELL__ < 705
import Data.Orphans ()
import Generics.Deriving.Base
#endif

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

---- Taken from GHC.Generics

-- Char
data D_Char
data C_Char

instance Datatype D_Char where
  datatypeName _ = "Char"
  moduleName   _ = "GHC.Base"

instance Constructor C_Char where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

type Rep0Char = D1 D_Char (C1 C_Char (S1 NoSelector (Rec0 Char)))
instance Generic Char where
  type Rep Char = Rep0Char
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x

-- Int
data D_Int
data C_Int

instance Datatype D_Int where
  datatypeName _ = "Int"
  moduleName   _ = "GHC.Int"

instance Constructor C_Int where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

type Rep0Int = D1 D_Int (C1 C_Int (S1 NoSelector (Rec0 Int)))
instance Generic Int where
  type Rep Int = Rep0Int
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x


-- Float
data D_Float
data C_Float

instance Datatype D_Float where
  datatypeName _ = "Float"
  moduleName   _ = "GHC.Float"

instance Constructor C_Float where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

type Rep0Float = D1 D_Float (C1 C_Float (S1 NoSelector (Rec0 Float)))
instance Generic Float where
  type Rep Float = Rep0Float
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x

-- etc...

-- Kind * -> *

-- Maybe a
type Rep0Maybe a = D1 D1Maybe (C1 C1_0Maybe U1
                           :+: C1 C1_1Maybe (S1 NoSelector (Rec0 a)))
instance Generic (Maybe a) where
    type Rep (Maybe a) = Rep0Maybe a

    from Nothing  = M1 (L1 (M1 U1))
    from (Just j) = M1 (R1 (M1 (M1 (K1 j))))

    to (M1 (L1 (M1 U1)))          = Nothing
    to (M1 (R1 (M1 (M1 (K1 j))))) = Just j

data D1Maybe
data C1_0Maybe
data C1_1Maybe

instance Datatype D1Maybe where
    datatypeName _ = "Maybe"
    moduleName   _ = "Data.Maybe"

instance Constructor C1_0Maybe where
    conName _ = "Nothing"

instance Constructor C1_1Maybe where
    conName _ = "Just"

-- [a]
type Rep0List a = D1 D1List (C1 C1_0List U1
                         :+: C1 C1_1List (S1 NoSelector (Rec0 a)
                                      :*: S1 NoSelector (Rec0 [a])))
instance Generic [a] where
    type Rep [a] = Rep0List a

    from []    = M1 (L1 (M1 U1))
    from (h:t) = M1 (R1 (M1 (M1 (K1 h) :*: M1 (K1 t))))

    to (M1 (L1 (M1 U1)))                        = []
    to (M1 (R1 (M1 (M1 (K1 h) :*: M1 (K1 t))))) = h : t

data D1List
data C1_0List
data C1_1List

instance Datatype D1List where
    datatypeName _ = "[]"
    moduleName   _ = "GHC.Types"

instance Constructor C1_0List where
    conName _ = "[]"

instance Constructor C1_1List where
    conName   _ = ":"
    conFixity _ = Infix RightAssociative 5
#endif

#if __GLASGOW_HASKELL__ >= 701 && __GLASGOW_HASKELL__ < 705
-- GHC 7.2 and 7.4 still need these instances; 7.6 doesn't

type Rep1Maybe = Rep1 Maybe
type Rep1List  = Rep1 []

-- etc...

#elif __GLASGOW_HASKELL__ < 701

type Rep1Maybe = D1 D1Maybe (C1 C1_0Maybe U1
                         :+: C1 C1_1Maybe (S1 NoSelector Par1))
instance Generic1 Maybe where
    type Rep1 Maybe = Rep1Maybe

    from1 Nothing  = M1 (L1 (M1 U1))
    from1 (Just j) = M1 (R1 (M1 (M1 (Par1 j))))

    to1 (M1 (L1 (M1 U1)))            = Nothing
    to1 (M1 (R1 (M1 (M1 (Par1 j))))) = Just j

type Rep1List = D1 D1List (C1 C1_0List U1 :+:
                           C1 C1_1List (S1 NoSelector Par1
                                    :*: S1 NoSelector (Rec1 [])))
instance Generic1 [] where
    type Rep1 [] = Rep1List

    from1 []    = M1 (L1 (M1 U1))
    from1 (h:t) = M1 (R1 (M1 (M1 (Par1 h) :*: M1 (Rec1 t))))

    to1 (M1 (L1 (M1 U1)))                            = []
    to1 (M1 (R1 (M1 (M1 (Par1 h) :*: M1 (Rec1 t))))) = h : t

#endif
