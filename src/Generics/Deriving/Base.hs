{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}

module Generics.Deriving.Base (
#ifndef __UHC__
  -- * Generic representation types
    V1, U1(..), Par1(..), Rec1(..), K1(..), M1(..)
  , (:+:)(..), (:*:)(..), (:.:)(..)

  -- ** Synonyms for convenience
  , Rec0, Par0, R, P
  , D1, C1, S1, D, C, S

  -- * Meta-information
  , Datatype(..), Constructor(..), Selector(..)
  , Fixity(..), Associativity(..)

  -- * Representable type classes
  , Representable0(..), Representable1(..)

  ,
#else
  module UHC.Generics,
#endif
  -- * Representations for base types
    Rep0Char, Rep0Int, Rep0Float
  , Rep0Maybe, Rep1Maybe
  , Rep0List_, Rep1List_

  ) where


#ifdef __UHC__
import UHC.Generics
#endif

#ifndef __UHC__
--------------------------------------------------------------------------------
-- Representation types
--------------------------------------------------------------------------------

-- | Void: used for datatypes without constructors
#ifdef __UHC__
V1 :: * -> *
#endif
data V1 p

-- | Unit: used for constructors without arguments
#ifdef __UHC__
U1 :: * -> *
#endif
data U1 p = U1

-- | Used for marking occurrences of the parameter
#ifdef __UHC__
Par1 :: * -> *
#endif
newtype Par1 p = Par1 { unPar1 :: p }


-- | Recursive calls of kind * -> *
#ifdef __UHC__
Rec1 :: (* -> *) -> * -> *
#endif
newtype Rec1 f p = Rec1 { unRec1 :: f p }

-- | Constants, additional parameters and recursion of kind *
#ifdef __UHC__
K1 :: * -> * -> * -> *
#endif
newtype K1 i c p = K1 { unK1 :: c }

-- | Meta-information (constructor names, etc.)
#ifdef __UHC__
M1 :: * -> * -> (* -> *) -> * -> *
#endif
newtype M1 i c f p = M1 { unM1 :: f p }

-- | Sums: encode choice between constructors
infixr 5 :+:
#ifdef __UHC__
(:+:) :: (* -> *) -> (* -> *) -> * -> *
#endif
data (:+:) f g p = L1 { unL1 :: f p } | R1 { unR1 :: g p }

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
#ifdef __UHC__
(:*:) :: (* -> *) -> (* -> *) -> * -> *
#endif
data (:*:) f g p = f p :*: g p

-- | Composition of functors
infixr 7 :.:
#ifdef __UHC__
(:.:) :: (* -> *) -> (* -> *) -> * -> *
#endif
newtype (:.:) f g p = Comp1 { unComp1 :: f (g p) }

-- | Tag for K1: recursion (of kind *)
data R
-- | Tag for K1: parameters (other than the last)
data P

-- | Type synonym for encoding recursion (of kind *)
type Rec0  = K1 R
-- | Type synonym for encoding parameters (other than the last)
type Par0  = K1 P

-- | Tag for M1: datatype
data D
-- | Tag for M1: constructor
data C
-- | Tag for M1: record selector
data S

-- | Type synonym for encoding meta-information for datatypes
type D1 = M1 D

-- | Type synonym for encoding meta-information for constructors
type C1 = M1 C

-- | Type synonym for encoding meta-information for record selectors
type S1 = M1 S

-- | Class for datatypes that represent datatypes
class Datatype d where
  -- | The name of the datatype, fully qualified
#ifdef __UHC__
  datatypeName :: t d f a -> String
  moduleName   :: t d f a -> String
#else
  datatypeName :: t d (f :: * -> *) a -> String
  moduleName   :: t d (f :: * -> *) a -> String
#endif

-- | Class for datatypes that represent records
class Selector s where
  -- | The name of the selector
#ifdef __UHC__
  selName :: t s f a -> String
#else
  selName :: t s (f :: * -> *) a -> String
#endif

-- | Class for datatypes that represent data constructors
class Constructor c where
  -- | The name of the constructor
#ifdef __UHC__
  conName :: t c f a -> String
#else
  conName :: t c (f :: * -> *) a -> String
#endif

  -- | The fixity of the constructor
#ifdef __UHC__
  conFixity :: t c f a -> Fixity
#else
  conFixity :: t c (f :: * -> *) a -> Fixity
#endif  
  conFixity = const Prefix

  -- | Marks if this constructor is a record
#ifdef __UHC__
  conIsRecord :: t c f a -> Bool
#else
  conIsRecord :: t c (f :: * -> *) a -> Bool
#endif
  conIsRecord = const False

-- | Datatype to represent the fixity of a constructor. An infix
-- | declaration directly corresponds to an application of 'Infix'.
data Fixity = Prefix | Infix Associativity Int
  deriving (Eq, Show, Ord, Read)

-- | Datatype to represent the associativy of a constructor
data Associativity =  LeftAssociative 
                   |  RightAssociative
                   |  NotAssociative
  deriving (Eq, Show, Ord, Read)

-- | Representable types of kind *
class Representable0 a rep where
  -- | Convert from the datatype to its representation
  from0  :: a -> rep x
  -- | Convert from the representation to the datatype
  to0    :: rep x -> a

-- | Representable types of kind * -> *
class Representable1 f rep where
  -- | Convert from the datatype to its representation
  from1  :: f a -> rep a
  -- | Convert from the representation to the datatype
  to1    :: rep a -> f a

#endif
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

type Rep0List_ a = D1 List__ ((C1 Nil__ U1) :+: (C1 Cons__ (Par0 a :*: Rec0 [a])))
instance Representable0 [a] (Rep0List_ a) where
  from0 []    = M1 (L1 (M1 U1))
  from0 (h:t) = M1 (R1 (M1 (K1 h :*: K1 t)))
  to0 (M1 (L1 (M1 U1)))              = []
  to0 (M1 (R1 (M1 (K1 h :*: K1 t)))) = h : t

type Rep1List_ = D1 List__ ((C1 Nil__ U1) :+: (C1 Cons__ (Par1 :*: Rec1 [])))
instance Representable1 [] Rep1List_ where
  from1 []    = M1 (L1 (M1 U1))
  from1 (h:t) = M1 (R1 (M1 (Par1 h :*: Rec1 t)))
  to1 (M1 (L1 (M1 U1)))                  = []
  to1 (M1 (R1 (M1 (Par1 h :*: Rec1 t)))) = h : t

-- etc...
