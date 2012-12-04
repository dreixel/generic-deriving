{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Generics.Deriving.Base (
#if __GLASGOW_HASKELL__ < 701
  -- * Generic representation types
    V1, U1(..), Par1(..), Rec1(..), K1(..), M1(..)
  , (:+:)(..), (:*:)(..), (:.:)(..)

  -- ** Synonyms for convenience
  , Rec0, Par0, R, P
  , D1, C1, S1, D, C, S

  -- * Meta-information
  , Datatype(..), Constructor(..), Selector(..), NoSelector
  , Fixity(..), Associativity(..), Arity(..), prec

  -- * Generic type classes
  , Generic(..), Generic1(..)

  ,
#else
  module GHC.Generics,
#endif

  ) where


#if __GLASGOW_HASKELL__ >= 701
import GHC.Generics

#else
--------------------------------------------------------------------------------
-- Representation types
--------------------------------------------------------------------------------

-- | Void: used for datatypes without constructors
data V1 p

-- | Unit: used for constructors without arguments
data U1 p = U1

-- | Used for marking occurrences of the parameter
newtype Par1 p = Par1 { unPar1 :: p }


-- | Recursive calls of kind * -> *
newtype Rec1 f p = Rec1 { unRec1 :: f p }

-- | Constants, additional parameters and recursion of kind *
newtype K1 i c p = K1 { unK1 :: c }

-- | Meta-information (constructor names, etc.)
newtype M1 i c f p = M1 { unM1 :: f p }

-- | Sums: encode choice between constructors
infixr 5 :+:
data (:+:) f g p = L1 { unL1 :: f p } | R1 { unR1 :: g p }

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
data (:*:) f g p = f p :*: g p

-- | Composition of functors
infixr 7 :.:
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
  datatypeName :: t d (f :: * -> *) a -> String
  moduleName   :: t d (f :: * -> *) a -> String

-- | Class for datatypes that represent records
class Selector s where
  -- | The name of the selector
  selName :: t s (f :: * -> *) a -> String

-- | Used for constructor fields without a name
data NoSelector

instance Selector NoSelector where selName _ = ""

-- | Class for datatypes that represent data constructors
class Constructor c where
  -- | The name of the constructor
  conName :: t c (f :: * -> *) a -> String

  -- | The fixity of the constructor
  conFixity :: t c (f :: * -> *) a -> Fixity
  conFixity = const Prefix

  -- | Marks if this constructor is a record
  conIsRecord :: t c (f :: * -> *) a -> Bool
  conIsRecord = const False


-- | Datatype to represent the arity of a tuple.
data Arity = NoArity | Arity Int
  deriving (Eq, Show, Ord, Read)

-- | Datatype to represent the fixity of a constructor. An infix
-- | declaration directly corresponds to an application of 'Infix'.
data Fixity = Prefix | Infix Associativity Int
  deriving (Eq, Show, Ord, Read)

-- | Get the precedence of a fixity value.
prec :: Fixity -> Int
prec Prefix      = 10
prec (Infix _ n) = n

-- | Datatype to represent the associativy of a constructor
data Associativity =  LeftAssociative 
                   |  RightAssociative
                   |  NotAssociative
  deriving (Eq, Show, Ord, Read)

-- | Representable types of kind *
class Generic a where
  type Rep a :: * -> *
  -- | Convert from the datatype to its representation
  from  :: a -> Rep a x
  -- | Convert from the representation to the datatype
  to    :: Rep a x -> a

-- | Representable types of kind * -> *
class Generic1 f where
  type Rep1 f :: * -> *
  -- | Convert from the datatype to its representation
  from1  :: f a -> Rep1 f a
  -- | Convert from the representation to the datatype
  to1    :: Rep1 f a -> f a

-- Instances for representation types
instance Functor U1 where
  fmap _ U1 = U1
instance Functor (K1 i a) where
  fmap _ (K1 x) = K1 x
instance Functor f => Functor (M1 i c f) where
  fmap f (M1 x) = M1 (fmap f x)
instance Functor Par1 where
  fmap f (Par1 p) = Par1 (f p)
instance Functor f => Functor (Rec1 f) where
  fmap f (Rec1 x) = Rec1 (fmap f x)
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (L1 x) = L1 (fmap f x)
  fmap f (R1 x) = R1 (fmap f x)
instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (x :*: y) = fmap f x :*: fmap f y
instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp1 x) = Comp1 (fmap (fmap f) x)

#endif
