{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 711
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

#if __GLASGOW_HASKELL__ >= 705
{-# LANGUAGE PolyKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generics.Deriving.Instances (
-- Only instances from Generics.Deriving.Base
-- and the Generic1 instances
#if !(MIN_VERSION_base(4,14,0))
    Rep0Kleisli
  , Rep1Kleisli
#endif
#if !(MIN_VERSION_base(4,12,0))
  , Rep0Down
  , Rep1Down
#endif
#if !(MIN_VERSION_base(4,9,0))
  , Rep0ExitCode
  , Rep0Version
  , Rep1ConSum
  , Rep1ConProduct
  , Rep1ConCompose
  , Rep1K1
  , Rep1M1
  , Rep1Par1
  , Rep1Rec1
  , Rep1U1
  , Rep0V1
  , Rep1V1
  , Rep0UAddr
  , Rep1UAddr
  , Rep0UChar
  , Rep1UChar
  , Rep0UDouble
  , Rep1UDouble
  , Rep0UFloat
  , Rep1UFloat
  , Rep0UInt
  , Rep1UInt
  , Rep0UWord
  , Rep1UWord
# if MIN_VERSION_base(4,4,0)
  , Rep0Complex
  , Rep1Complex
# endif
# if MIN_VERSION_base(4,7,0)
  , Rep1Proxy
# endif
#endif
#if !(MIN_VERSION_base(4,7,0))
  , Rep0All
  , Rep0Any
  , Rep0Arity
  , Rep0Associativity
  , Rep0Const
  , Rep1Const
  , Rep0Dual
  , Rep1Dual
  , Rep0Endo
  , Rep0First
  , Rep1First
  , Rep0Fixity
  , Rep0Last
  , Rep1Last
  , Rep0Product
  , Rep1Product
  , Rep0Sum
  , Rep1Sum
  , Rep0WrappedArrow
  , Rep1WrappedArrow
  , Rep0WrappedMonad
  , Rep1WrappedMonad
  , Rep0ZipList
  , Rep1ZipList
  , Rep0U1
  , Rep0Par1
  , Rep0Rec1
  , Rep0K1
  , Rep0M1
  , Rep0ConSum
  , Rep0ConProduct
  , Rep0ConCompose
#endif
#if !(MIN_VERSION_base(4,6,0))
  , Rep1Either
  , Rep1List
  , Rep1Maybe
  , Rep1Tuple2
  , Rep1Tuple3
  , Rep1Tuple4
  , Rep1Tuple5
  , Rep1Tuple6
  , Rep1Tuple7
#endif
#if !(MIN_VERSION_base(4,4,0))
  , Rep0Bool
  , Rep0Char
  , Rep0Double
  , Rep0Either
  , Rep0Int
  , Rep0Float
  , Rep0List
  , Rep0Maybe
  , Rep0Ordering
  , Rep0Tuple2
  , Rep0Tuple3
  , Rep0Tuple4
  , Rep0Tuple5
  , Rep0Tuple6
  , Rep0Tuple7
  , Rep0Unit
#endif
  ) where

#if !(MIN_VERSION_base(4,7,0))
import Control.Applicative
import Data.Monoid
#endif

#if MIN_VERSION_base(4,4,0) && !(MIN_VERSION_base(4,9,0))
import Data.Complex (Complex(..))
#endif

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,9,0))
import Data.Proxy (Proxy(..))
#endif

#if !(MIN_VERSION_base(4,9,0))
import Data.Version (Version(..))
import System.Exit (ExitCode(..))
#endif

#if !(MIN_VERSION_base(4,12,0))
# if MIN_VERSION_base(4,6,0)
import Data.Ord (Down(..))
# else
import GHC.Exts (Down(..))
# endif
#endif

#if !(MIN_VERSION_base(4,14,0))
import Control.Arrow (Kleisli(..))
#endif

#if !(MIN_VERSION_base(4,14,0))
import Generics.Deriving.Base.Internal
#endif

#if !(MIN_VERSION_base(4,14,0))
# if MIN_VERSION_base(4,6,0)
type Rep0Kleisli m a b = Rep  (Kleisli m a b)
type Rep1Kleisli m a   = Rep1 (Kleisli m a)
deriving instance Generic  (Kleisli m a b)
deriving instance Generic1 (Kleisli m a)
# else
type Rep0Kleisli m a b = D1 D1Kleisli (C1 C1_0Kleisli (S1 S1_0_0Kleisli (Rec0 (a -> m b))))
type Rep1Kleisli m a   = D1 D1Kleisli (C1 C1_0Kleisli (S1 S1_0_0Kleisli ((->) a :.: Rec1 m)))

instance Generic (Kleisli m a b) where
    type Rep (Kleisli m a b) = Rep0Kleisli m a b
    from x = M1 (case x of
        Kleisli g -> M1 (M1 (K1 g)))
    to (M1 x) = case x of
        M1 (M1 (K1 g)) -> Kleisli g

instance Generic1 (Kleisli m a) where
    type Rep1 (Kleisli m a) = Rep1Kleisli m a
    from1 x = M1 (case x of
        Kleisli g -> M1 (M1 (Comp1 (fmap Rec1 g))))
    to1 (M1 x) = case x of
        M1 (M1 g) -> Kleisli (fmap unRec1 (unComp1 g))

data D1Kleisli
data C1_0Kleisli
data S1_0_0Kleisli

instance Datatype D1Kleisli where
  datatypeName _ = "Kleisli"
  moduleName   _ = "Control.Arrow"

instance Constructor C1_0Kleisli where
  conName     _ = "Kleisli"
  conIsRecord _ = True

instance Selector S1_0_0Kleisli where
  selName _ = "runKleisli"
# endif
#endif

#if !(MIN_VERSION_base(4,12,0))
# if MIN_VERSION_base(4,6,0)
type Rep0Down a = Rep (Down a)
type Rep1Down   = Rep1 Down
deriving instance Generic (Down a)
deriving instance Generic1 Down
# else
type Rep0Down a = D1 D1Down (C1 C1_0Down (S1 NoSelector (Rec0 a)))
type Rep1Down   = D1 D1Down (C1 C1_0Down (S1 NoSelector Par1))

instance Generic (Down a) where
    type Rep (Down a) = Rep0Down a
    from x = M1 (case x of
        Down g -> M1 (M1 (K1 g)))
    to (M1 x) = case x of
        M1 (M1 (K1 g)) -> Down g

instance Generic1 Down where
    type Rep1 Down = Rep1Down
    from1 x = M1 (case x of
        Down g -> M1 (M1 (Par1 g)))
    to1 (M1 x) = case x of
        M1 (M1 g) -> Down (unPar1 g)

data D1Down
data C1_0Down

instance Datatype D1Down where
    datatypeName _ = "Down"
    moduleName   _ = "GHC.Exts"

instance Constructor C1_0Down where
    conName _ = "Down"
# endif
#endif

-----

#if !(MIN_VERSION_base(4,9,0))
type Rep0ExitCode = D1 D1ExitCode (C1 C1_0ExitCode U1
                               :+: C1 C1_1ExitCode (S1 NoSelector (Rec0 Int)))

instance Generic ExitCode where
    type Rep ExitCode = Rep0ExitCode
    from x = M1 (case x of
        ExitSuccess   -> L1 (M1 U1)
        ExitFailure g -> R1 (M1 (M1 (K1 g))))
    to (M1 x) = case x of
        L1 (M1 U1)          -> ExitSuccess
        R1 (M1 (M1 (K1 g))) -> ExitFailure g

data D1ExitCode
data C1_0ExitCode
data C1_1ExitCode

instance Datatype D1ExitCode where
    datatypeName _ = "ExitCode"
    moduleName   _ = "GHC.IO.Exception"

instance Constructor C1_0ExitCode where
    conName _ = "ExitSuccess"

instance Constructor C1_1ExitCode where
    conName _ = "ExitFailure"

-----

type Rep0Version = D1 D1Version (C1 C1_0Version (S1 S1_0_0Version (Rec0 [Int])
                                             :*: S1 S1_0_1Version (Rec0 [String])))

instance Generic Version where
    type Rep Version = Rep0Version
    from (Version b t) = M1 (M1 (M1 (K1 b) :*: M1 (K1 t)))
    to (M1 (M1 (M1 (K1 b) :*: M1 (K1 t)))) = Version b t

data D1Version
data C1_0Version
data S1_0_0Version
data S1_0_1Version

instance Datatype D1Version where
    datatypeName _ = "Version"
    moduleName   _ = "Data.Version"

instance Constructor C1_0Version where
    conName     _ = "Version"
    conIsRecord _ = True

instance Selector S1_0_0Version where
    selName _ = "versionBranch"

instance Selector S1_0_1Version where
    selName _ = "versionTags"

-----

type Rep1ConSum f g = D1 D1ConSum (C1 C1_0ConSum (S1 NoSelector (Rec1 f))
                               :+: C1 C1_1ConSum (S1 NoSelector (Rec1 g)))

instance Generic1 (f :+: g) where
    type Rep1 (f :+: g) = Rep1ConSum f g
    from1 x = M1 (case x of
        L1 l -> L1 (M1 (M1 (Rec1 l)))
        R1 r -> R1 (M1 (M1 (Rec1 r))))
    to1 (M1 x) = case x of
        L1 (M1 (M1 l)) -> L1 (unRec1 l)
        R1 (M1 (M1 r)) -> R1 (unRec1 r)

data D1ConSum
data C1_0ConSum
data C1_1ConSum

instance Datatype D1ConSum where
    datatypeName _ = ":+:"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

instance Constructor C1_0ConSum where
    conName _ = "L1"

instance Constructor C1_1ConSum where
    conName _ = "R1"

-----

type Rep1ConProduct f g = D1 D1ConProduct (C1 C1_ConProduct (S1 NoSelector (Rec1 f)
                                                         :*: S1 NoSelector (Rec1 g)))

instance Generic1 (f :*: g) where
    type Rep1 (f :*: g) = Rep1ConProduct f g
    from1 (f :*: g) = M1 (M1 (M1 (Rec1 f) :*: M1 (Rec1 g)))
    to1 (M1 (M1 (M1 f :*: M1 g))) = unRec1 f :*: unRec1 g

data D1ConProduct
data C1_ConProduct

instance Datatype D1ConProduct where
    datatypeName _ = ":*:"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

instance Constructor C1_ConProduct where
    conName   _ = ":*:"
    conFixity _ = Infix RightAssociative 6

-----

type Rep1ConCompose f g =
    D1 D1ConCompose (C1 C1_0ConCompose (S1 S1_0_0ConCompose (f :.: Rec1 g)))

instance Functor f => Generic1 (f :.: g) where
    type Rep1 (f :.: g) = Rep1ConCompose f g
    from1 (Comp1 c) = M1 (M1 (M1 (Comp1 (fmap Rec1 c))))
    to1 (M1 (M1 (M1 c))) = Comp1 (fmap unRec1 (unComp1 c))

data D1ConCompose
data C1_0ConCompose
data S1_0_0ConCompose

instance Datatype D1ConCompose where
    datatypeName _ = ":.:"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

instance Constructor C1_0ConCompose where
  conName     _ = "Comp1"
  conIsRecord _ = True

instance Selector S1_0_0ConCompose where
  selName _ = "unComp1"

-----

type Rep1K1 i c = D1 D1K1 (C1 C1_0K1 (S1 S1_0_0K1 (Rec0 c)))

instance Generic1 (K1 i c) where
    type Rep1 (K1 i c) = Rep1K1 i c
    from1 (K1 c) = M1 (M1 (M1 (K1 c)))
    to1 (M1 (M1 (M1 c))) = K1 (unK1 c)

data D1K1
data C1_0K1
data S1_0_0K1

instance Datatype D1K1 where
    datatypeName _ = "K1"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

instance Constructor C1_0K1 where
    conName     _ = "K1"
    conIsRecord _ = True

instance Selector S1_0_0K1 where
    selName _ = "unK1"

-----

type Rep1M1 i c f = D1 D1M1 (C1 C1_0M1 (S1 S1_0_0M1 (Rec1 f)))

instance Generic1 (M1 i c f) where
    type Rep1 (M1 i c f) = Rep1M1 i c f
    from1 (M1 m) = M1 (M1 (M1 (Rec1 m)))
    to1 (M1 (M1 (M1 m))) = M1 (unRec1 m)

data D1M1
data C1_0M1
data S1_0_0M1

instance Datatype D1M1 where
    datatypeName _ = "M1"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

instance Constructor C1_0M1 where
    conName     _ = "M1"
    conIsRecord _ = True

instance Selector S1_0_0M1 where
    selName _ = "unM1"

-----

type Rep1Par1 = D1 D1Par1 (C1 C1_0Par1 (S1 S1_0_0Par1 Par1))

instance Generic1 Par1 where
    type Rep1 Par1 = Rep1Par1
    from1 (Par1 p) = M1 (M1 (M1 (Par1 p)))
    to1 (M1 (M1 (M1 p))) = Par1 (unPar1 p)

data D1Par1
data C1_0Par1
data S1_0_0Par1

instance Datatype D1Par1 where
    datatypeName _ = "Par1"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

instance Constructor C1_0Par1 where
    conName     _ = "Par1"
    conIsRecord _ = True

instance Selector S1_0_0Par1 where
    selName _ = "unPar1"

-----

type Rep1Rec1 f = D1 D1Rec1 (C1 C1_0Rec1 (S1 S1_0_0Rec1 (Rec1 f)))

instance Generic1 (Rec1 f) where
    type Rep1 (Rec1 f) = Rep1Rec1 f
    from1 (Rec1 r) = M1 (M1 (M1 (Rec1 r)))
    to1 (M1 (M1 (M1 r))) = Rec1 (unRec1 r)

data D1Rec1
data C1_0Rec1
data S1_0_0Rec1

instance Datatype D1Rec1 where
    datatypeName _ = "Rec1"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

instance Constructor C1_0Rec1 where
    conName     _ = "Rec1"
    conIsRecord _ = True

instance Selector S1_0_0Rec1 where
    selName _ = "unRec1"

-----

type Rep1U1 = D1 D1U1 (C1 C1_0U1 U1)

instance Generic1 U1 where
    type Rep1 U1 = Rep1U1
    from1 U1 = M1 (M1 U1)
    to1 (M1 (M1 U1)) = U1

data D1U1
data C1_0U1

instance Datatype D1U1 where
    datatypeName _ = "U1"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

instance Constructor C1_0U1 where
    conName _ = "U1"

-----

type Rep0V1 p = D1 D1V1 V1
type Rep1V1   = D1 D1V1 V1

instance Generic (V1 p) where
    type Rep (V1 p) = Rep0V1 p
    from x = M1 (case x of !_ -> error "No generic representation for empty datatype V1")
    to (M1 !_) = error "No values for empty datatype V1"

instance Generic1 V1 where
    type Rep1 V1 = Rep1V1
    from1 x = M1 (case x of !_ -> error "No generic representation for empty datatype V1")
    to1 (M1 !_) = error "No values for empty datatype V1"

data D1V1

instance Datatype D1V1 where
    datatypeName _ = "V1"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

-----

type Rep0UAddr p = D1 D1UAddr (C1 C1_0UAddr (S1 S1_0_0UAddr UAddr))
type Rep1UAddr   = D1 D1UAddr (C1 C1_0UAddr (S1 S1_0_0UAddr UAddr))

instance Generic (UAddr p) where
    type Rep (UAddr p) = Rep0UAddr p
    from (UAddr a) = M1 (M1 (M1 (UAddr a)))
    to (M1 (M1 (M1 (UAddr a)))) = UAddr a

instance Generic1 UAddr where
    type Rep1 UAddr = Rep1UAddr
    from1 (UAddr a) = M1 (M1 (M1 (UAddr a)))
    to1 (M1 (M1 (M1 (UAddr a)))) = UAddr a

data D1UAddr
data C1_0UAddr
data S1_0_0UAddr

instance Datatype D1UAddr where
    datatypeName _ = "UAddr"
    moduleName   _ = "Generics.Deriving.Base.Internal"

instance Constructor C1_0UAddr where
    conName     _ = "UAddr"
    conIsRecord _ = True

instance Selector S1_0_0UAddr where
    selName _ = "uAddr#"

-----

type Rep0UChar p = D1 D1UChar (C1 C1_0UChar (S1 S1_0_0UChar UChar))
type Rep1UChar   = D1 D1UChar (C1 C1_0UChar (S1 S1_0_0UChar UChar))

instance Generic (UChar p) where
    type Rep (UChar p) = Rep0UChar p
    from (UChar c) = M1 (M1 (M1 (UChar c)))
    to (M1 (M1 (M1 (UChar c)))) = UChar c

instance Generic1 UChar where
    type Rep1 UChar = Rep1UChar
    from1 (UChar c) = M1 (M1 (M1 (UChar c)))
    to1 (M1 (M1 (M1 (UChar c)))) = UChar c

data D1UChar
data C1_0UChar
data S1_0_0UChar

instance Datatype D1UChar where
    datatypeName _ = "UChar"
    moduleName   _ = "Generics.Deriving.Base.Internal"

instance Constructor C1_0UChar where
    conName     _ = "UChar"
    conIsRecord _ = True

instance Selector S1_0_0UChar where
    selName _ = "uChar#"

-----

type Rep0UDouble p = D1 D1UDouble (C1 C1_0UDouble (S1 S1_0_0UDouble UDouble))
type Rep1UDouble   = D1 D1UDouble (C1 C1_0UDouble (S1 S1_0_0UDouble UDouble))

instance Generic (UDouble p) where
    type Rep (UDouble p) = Rep0UDouble p
    from (UDouble d) = M1 (M1 (M1 (UDouble d)))
    to (M1 (M1 (M1 (UDouble d)))) = UDouble d

instance Generic1 UDouble where
    type Rep1 UDouble = Rep1UDouble
    from1 (UDouble d) = M1 (M1 (M1 (UDouble d)))
    to1 (M1 (M1 (M1 (UDouble d)))) = UDouble d

data D1UDouble
data C1_0UDouble
data S1_0_0UDouble

instance Datatype D1UDouble where
    datatypeName _ = "UDouble"
    moduleName   _ = "Generics.Deriving.Base.Internal"

instance Constructor C1_0UDouble where
    conName     _ = "UDouble"
    conIsRecord _ = True

instance Selector S1_0_0UDouble where
    selName _ = "uDouble#"

-----

type Rep0UFloat p = D1 D1UFloat (C1 C1_0UFloat (S1 S1_0_0UFloat UFloat))
type Rep1UFloat   = D1 D1UFloat (C1 C1_0UFloat (S1 S1_0_0UFloat UFloat))

instance Generic (UFloat p) where
    type Rep (UFloat p) = Rep0UFloat p
    from (UFloat f) = M1 (M1 (M1 (UFloat f)))
    to (M1 (M1 (M1 (UFloat f)))) = UFloat f

instance Generic1 UFloat where
    type Rep1 UFloat = Rep1UFloat
    from1 (UFloat f) = M1 (M1 (M1 (UFloat f)))
    to1 (M1 (M1 (M1 (UFloat f)))) = UFloat f

data D1UFloat
data C1_0UFloat
data S1_0_0UFloat

instance Datatype D1UFloat where
    datatypeName _ = "UFloat"
    moduleName   _ = "Generics.Deriving.Base.Internal"

instance Constructor C1_0UFloat where
    conName     _ = "UFloat"
    conIsRecord _ = True

instance Selector S1_0_0UFloat where
    selName _ = "uFloat#"

-----

type Rep0UInt p = D1 D1UInt (C1 C1_0UInt (S1 S1_0_0UInt UInt))
type Rep1UInt   = D1 D1UInt (C1 C1_0UInt (S1 S1_0_0UInt UInt))

instance Generic (UInt p) where
    type Rep (UInt p) = Rep0UInt p
    from (UInt i) = M1 (M1 (M1 (UInt i)))
    to (M1 (M1 (M1 (UInt i)))) = UInt i

instance Generic1 UInt where
    type Rep1 UInt = Rep1UInt
    from1 (UInt i) = M1 (M1 (M1 (UInt i)))
    to1 (M1 (M1 (M1 (UInt i)))) = UInt i

data D1UInt
data C1_0UInt
data S1_0_0UInt

instance Datatype D1UInt where
    datatypeName _ = "UInt"
    moduleName   _ = "Generics.Deriving.Base.Internal"

instance Constructor C1_0UInt where
    conName     _ = "UInt"
    conIsRecord _ = True

instance Selector S1_0_0UInt where
    selName _ = "uInt#"

-----

type Rep0UWord p = D1 D1UWord (C1 C1_0UWord (S1 S1_0_0UWord UWord))
type Rep1UWord   = D1 D1UWord (C1 C1_0UWord (S1 S1_0_0UWord UWord))

instance Generic (UWord p) where
    type Rep (UWord p) = Rep0UWord p
    from (UWord w) = M1 (M1 (M1 (UWord w)))
    to (M1 (M1 (M1 (UWord w)))) = UWord w

instance Generic1 UWord where
    type Rep1 UWord = Rep1UWord
    from1 (UWord w) = M1 (M1 (M1 (UWord w)))
    to1 (M1 (M1 (M1 (UWord w)))) = UWord w

data D1UWord
data C1_0UWord
data S1_0_0UWord

instance Datatype D1UWord where
    datatypeName _ = "UWord"
    moduleName   _ = "Generics.Deriving.Base.Internal"

instance Constructor C1_0UWord where
    conName     _ = "UWord"
    conIsRecord _ = True

instance Selector S1_0_0UWord where
    selName _ = "uWord#"

-----

# if MIN_VERSION_base(4,4,0)
type Rep0Complex a = D1 D1Complex (C1 C1_0Complex (S1 NoSelector (Rec0 a)
                                               :*: S1 NoSelector (Rec0 a)))
type Rep1Complex = D1 D1Complex (C1 C1_0Complex (S1 NoSelector Par1
                                             :*: S1 NoSelector Par1))

instance Generic (Complex a) where
    type Rep (Complex a) = Rep0Complex a
    from (a :+ b) = M1 (M1 (M1 (K1 a) :*: M1 (K1 b)))
    to (M1 (M1 (M1 (K1 a) :*: M1 (K1 b)))) = a :+ b

instance Generic1 Complex where
    type Rep1 Complex = Rep1Complex
    from1 (a :+ b) = M1 (M1 (M1 (Par1 a) :*: M1 (Par1 b)))
    to1 (M1 (M1 (M1 a :*: M1 b))) = unPar1 a :+ unPar1 b

data D1Complex
data C1_0Complex

instance Datatype D1Complex where
    datatypeName _ = "Complex"
    moduleName   _ = "Data.Complex"

instance Constructor C1_0Complex where
    conName   _ = ":+"
    conFixity _ = Infix LeftAssociative 9
# endif

-----

# if MIN_VERSION_base(4,7,0)
type Rep1Proxy = D1 D1Proxy (C1 C1_0Proxy U1)

instance Generic1 Proxy where
    type Rep1 Proxy = Rep1Proxy
    from1 Proxy      = M1 (M1 U1)
    to1 (M1 (M1 U1)) = Proxy

data D1Proxy
data C1_0Proxy

instance Datatype D1Proxy where
    datatypeName _ = "Proxy"
    moduleName   _ = "Data.Proxy"

instance Constructor C1_0Proxy where
    conName _ = "Proxy"
# endif
#endif

-----

#if !(MIN_VERSION_base(4,7,0))
--------------------------------------------------------------------------------
-- Representations for base types
--------------------------------------------------------------------------------

type Rep0All = D1 D1All (C1 C1_0All (S1 S1_0_0All (Rec0 Bool)))

instance Generic All where
    type Rep All = Rep0All
    from (All a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = All a

data D1All
data C1_0All
data S1_0_0All

instance Datatype D1All where
    datatypeName _ = "All"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0All where
    conName     _ = "All"
    conIsRecord _ = True

instance Selector S1_0_0All where
    selName _ = "getAll"

-----

type Rep0Any = D1 D1Any (C1 C1_0Any (S1 S1_0_0Any (Rec0 Bool)))

instance Generic Any where
    type Rep Any = Rep0Any
    from (Any a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = Any a

data D1Any
data C1_0Any
data S1_0_0Any

instance Datatype D1Any where
    datatypeName _ = "Any"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Any where
    conName     _ = "Any"
    conIsRecord _ = True

instance Selector S1_0_0Any where
    selName _ = "getAny"

-----

type Rep0Arity = D1 D1Arity (C1 C1_0Arity U1
                         :+: C1 C1_1Arity (S1 NoSelector (Rec0 Int)))

instance Generic Arity where
    type Rep Arity = Rep0Arity

    from x = M1 (case x of
        NoArity -> L1 (M1 U1)
        Arity a -> R1 (M1 (M1 (K1 a))))

    to (M1 x) = case x of
        L1 (M1 U1)          -> NoArity
        R1 (M1 (M1 (K1 a))) -> Arity a

data D1Arity
data C1_0Arity
data C1_1Arity

instance Datatype D1Arity where
    datatypeName _ = "Arity"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

instance Constructor C1_0Arity where
    conName _ = "NoArity"

instance Constructor C1_1Arity where
    conName _ = "Arity"

-----

type Rep0Associativity = D1 D1Associativity (C1 C1_0Associativity U1
                                        :+: (C1 C1_1Associativity U1
                                        :+:  C1 C1_2Associativity U1))

instance Generic Associativity where
    type Rep Associativity = Rep0Associativity

    from x = M1 (case x of
        LeftAssociative  -> L1 (M1 U1)
        RightAssociative -> R1 (L1 (M1 U1))
        NotAssociative   -> R1 (R1 (M1 U1)))

    to (M1 x) = case x of
        L1 (M1 U1)      -> LeftAssociative
        R1 (L1 (M1 U1)) -> RightAssociative
        R1 (R1 (M1 U1)) -> NotAssociative

data D1Associativity
data C1_0Associativity
data C1_1Associativity
data C1_2Associativity

instance Datatype D1Associativity where
    datatypeName _ = "Associativity"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

instance Constructor C1_0Associativity where
    conName _ = "LeftAssociative"

instance Constructor C1_1Associativity where
    conName _ = "RightAssociative"

instance Constructor C1_2Associativity where
    conName _ = "NotAssociative"

-----

type Rep0Const a b = D1 D1Const (C1 C1_0Const (S1 S1_0_0Const (Rec0 a)))
type Rep1Const a   = D1 D1Const (C1 C1_0Const (S1 S1_0_0Const (Rec0 a)))

instance Generic (Const a b) where
    type Rep (Const a b) = Rep0Const a b
    from (Const a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = Const a

instance Generic1 (Const a) where
    type Rep1 (Const a) = Rep1Const a
    from1 (Const a) = M1 (M1 (M1 (K1 a)))
    to1 (M1 (M1 (M1 (K1 a)))) = Const a

data D1Const
data C1_0Const
data S1_0_0Const

instance Datatype D1Const where
    datatypeName _ = "Const"
    moduleName   _ = "Control.Applicative"

instance Constructor C1_0Const where
    conName     _ = "Const"
    conIsRecord _ = True

instance Selector S1_0_0Const where
    selName _ = "getConst"

-----

type Rep0Dual a = D1 D1Dual (C1 C1_0Dual (S1 S1_0_0Dual (Rec0 a)))
type Rep1Dual   = D1 D1Dual (C1 C1_0Dual (S1 S1_0_0Dual Par1))

instance Generic (Dual a) where
    type Rep (Dual a) = Rep0Dual a
    from (Dual d) = M1 (M1 (M1 (K1 d)))
    to (M1 (M1 (M1 (K1 d)))) = Dual d

instance Generic1 Dual where
    type Rep1 Dual = Rep1Dual
    from1 (Dual d) = M1 (M1 (M1 (Par1 d)))
    to1 (M1 (M1 (M1 (Par1 d)))) = Dual d

data D1Dual
data C1_0Dual
data S1_0_0Dual

instance Datatype D1Dual where
    datatypeName _ = "Dual"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Dual where
    conName     _ = "Dual"
    conIsRecord _ = True

instance Selector S1_0_0Dual where
    selName _ = "getDual"

-----

type Rep0Endo a = D1 D1Endo (C1 C1_0Endo (S1 S1_0_0Endo (Rec0 (a -> a))))

instance Generic (Endo a) where
    type Rep (Endo a) = Rep0Endo a
    from (Endo e) = M1 (M1 (M1 (K1 e)))
    to (M1 (M1 (M1 (K1 e)))) = Endo e

data D1Endo
data C1_0Endo
data S1_0_0Endo

instance Datatype D1Endo where
    datatypeName _ = "Endo"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Endo where
    conName     _ = "Endo"
    conIsRecord _ = True

instance Selector S1_0_0Endo where
    selName _ = "appEndo"

-----

type Rep0First a = D1 D1First (C1 C1_0First (S1 S1_0_0First (Rec0 (Maybe a))))
type Rep1First   = D1 D1First (C1 C1_0First (S1 S1_0_0First (Rec1 Maybe)))

instance Generic (First a) where
    type Rep (First a) = Rep0First a
    from (First f) = M1 (M1 (M1 (K1 f)))
    to (M1 (M1 (M1 (K1 f)))) = First f

instance Generic1 First where
    type Rep1 First = Rep1First
    from1 (First f) = M1 (M1 (M1 (Rec1 f)))
    to1 (M1 (M1 (M1 (Rec1 f)))) = First f

data D1First
data C1_0First
data S1_0_0First

instance Datatype D1First where
    datatypeName _ = "First"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0First where
    conName     _ = "First"
    conIsRecord _ = True

instance Selector S1_0_0First where
    selName _ = "getFirst"

-----

type Rep0Fixity = D1 D1Fixity (C1 C1_0Fixity U1
                           :+: C1 C1_1Fixity (S1 NoSelector (Rec0 Associativity)
                                          :*: S1 NoSelector (Rec0 Int)))

instance Generic Fixity where
    type Rep Fixity = Rep0Fixity

    from x = M1 (case x of
        Prefix    -> L1 (M1 U1)
        Infix a i -> R1 (M1 (M1 (K1 a) :*: M1 (K1 i))))

    to (M1 x) = case x of
        L1 (M1 U1)                        -> Prefix
        R1 (M1 (M1 (K1 a) :*: M1 (K1 i))) -> Infix a i

data D1Fixity
data C1_0Fixity
data C1_1Fixity

instance Datatype D1Fixity where
    datatypeName _ = "Fixity"
# if !(MIN_VERSION_base(4,4,0))
    moduleName   _ = "Generics.Deriving.Base.Internal"
# else
    moduleName   _ = "GHC.Generics"
# endif

instance Constructor C1_0Fixity where
    conName _ = "Prefix"

instance Constructor C1_1Fixity where
    conName _ = "Infix"

-----

type Rep0Last a = D1 D1Last (C1 C1_0Last (S1 S1_0_0Last (Rec0 (Maybe a))))
type Rep1Last   = D1 D1Last (C1 C1_0Last (S1 S1_0_0Last (Rec1 Maybe)))

instance Generic (Last a) where
    type Rep (Last a) = Rep0Last a
    from (Last l) = M1 (M1 (M1 (K1 l)))
    to (M1 (M1 (M1 (K1 l)))) = Last l

instance Generic1 Last where
    type Rep1 Last = Rep1Last
    from1 (Last l) = M1 (M1 (M1 (Rec1 l)))
    to1 (M1 (M1 (M1 (Rec1 l)))) = Last l

data D1Last
data C1_0Last
data S1_0_0Last

instance Datatype D1Last where
    datatypeName _ = "Last"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Last where
    conName     _ = "Last"
    conIsRecord _ = True

instance Selector S1_0_0Last where
    selName _ = "getLast"

-----

type Rep0Product a = D1 D1Product (C1 C1_0Product (S1 S1_0_0Product (Rec0 a)))
type Rep1Product   = D1 D1Product (C1 C1_0Product (S1 S1_0_0Product Par1))

instance Generic (Product a) where
    type Rep (Product a) = Rep0Product a
    from (Product p) = M1 (M1 (M1 (K1 p)))
    to (M1 (M1 (M1 (K1 p)))) = Product p

instance Generic1 Product where
    type Rep1 Product = Rep1Product
    from1 (Product p) = M1 (M1 (M1 (Par1 p)))
    to1 (M1 (M1 (M1 (Par1 p)))) = Product p

data D1Product
data C1_0Product
data S1_0_0Product

instance Datatype D1Product where
    datatypeName _ = "Product"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Product where
    conName     _ = "Product"
    conIsRecord _ = True

instance Selector S1_0_0Product where
    selName _ = "getProduct"

-----

type Rep0Sum a = D1 D1Sum (C1 C1_0Sum (S1 S1_0_0Sum (Rec0 a)))
type Rep1Sum   = D1 D1Sum (C1 C1_0Sum (S1 S1_0_0Sum Par1))

instance Generic (Sum a) where
    type Rep (Sum a) = Rep0Sum a
    from (Sum s) = M1 (M1 (M1 (K1 s)))
    to (M1 (M1 (M1 (K1 s)))) = Sum s

instance Generic1 Sum where
    type Rep1 Sum = Rep1Sum
    from1 (Sum s) = M1 (M1 (M1 (Par1 s)))
    to1 (M1 (M1 (M1 (Par1 s)))) = Sum s

data D1Sum
data C1_0Sum
data S1_0_0Sum

instance Datatype D1Sum where
    datatypeName _ = "Sum"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Sum where
    conName     _ = "Sum"
    conIsRecord _ = True

instance Selector S1_0_0Sum where
    selName _ = "getSum"

-----

type Rep0WrappedArrow a b c =
    D1 D1WrappedArrow (C1 C1_0WrappedArrow (S1 S1_0_0WrappedArrow (Rec0 (a b c))))
type Rep1WrappedArrow a b =
    D1 D1WrappedArrow (C1 C1_0WrappedArrow (S1 S1_0_0WrappedArrow (Rec1 (a b))))

instance Generic (WrappedArrow a b c) where
    type Rep (WrappedArrow a b c) = Rep0WrappedArrow a b c
    from (WrapArrow a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = WrapArrow a

instance Generic1 (WrappedArrow a b) where
    type Rep1 (WrappedArrow a b) = Rep1WrappedArrow a b
    from1 (WrapArrow a) = M1 (M1 (M1 (Rec1 a)))
    to1 (M1 (M1 (M1 (Rec1 a)))) = WrapArrow a

data D1WrappedArrow
data C1_0WrappedArrow
data S1_0_0WrappedArrow

instance Datatype D1WrappedArrow where
  datatypeName _ = "WrappedArrow"
  moduleName   _ = "Control.Applicative"

instance Constructor C1_0WrappedArrow where
  conName     _ = "WrapArrow"
  conIsRecord _ = True

instance Selector S1_0_0WrappedArrow where
  selName _ = "unwrapArrow"

-----

type Rep0WrappedMonad m a =
    D1 D1WrappedMonad (C1 C1_0WrappedMonad (S1 S1_0_0WrappedMonad (Rec0 (m a))))
type Rep1WrappedMonad m =
    D1 D1WrappedMonad (C1 C1_0WrappedMonad (S1 S1_0_0WrappedMonad (Rec1 m)))

instance Generic (WrappedMonad m a) where
    type Rep (WrappedMonad m a) = Rep0WrappedMonad m a
    from (WrapMonad m) = M1 (M1 (M1 (K1 m)))
    to (M1 (M1 (M1 (K1 m)))) = WrapMonad m

instance Generic1 (WrappedMonad m) where
    type Rep1 (WrappedMonad m) = Rep1WrappedMonad m
    from1 (WrapMonad m) = M1 (M1 (M1 (Rec1 m)))
    to1 (M1 (M1 (M1 (Rec1 m)))) = WrapMonad m

data D1WrappedMonad
data C1_0WrappedMonad
data S1_0_0WrappedMonad

instance Datatype D1WrappedMonad where
    datatypeName _ = "WrappedMonad"
    moduleName   _ = "Control.Applicative"

instance Constructor C1_0WrappedMonad where
    conName     _ = "WrapMonad"
    conIsRecord _ = True

instance Selector S1_0_0WrappedMonad where
    selName _ = "unwrapMonad"

-----

type Rep0ZipList a = D1 D1ZipList (C1 C1_0ZipList (S1 S1_0_0ZipList (Rec0 [a])))
type Rep1ZipList   = D1 D1ZipList (C1 C1_0ZipList (S1 S1_0_0ZipList (Rec1 [])))

instance Generic (ZipList a) where
    type Rep (ZipList a) = Rep0ZipList a
    from (ZipList z) = M1 (M1 (M1 (K1 z)))
    to (M1 (M1 (M1 (K1 z)))) = ZipList z

instance Generic1 ZipList where
    type Rep1 ZipList = Rep1ZipList
    from1 (ZipList z) = M1 (M1 (M1 (Rec1 z)))
    to1 (M1 (M1 (M1 (Rec1 z)))) = ZipList z

data D1ZipList
data C1_0ZipList
data S1_0_0ZipList

instance Datatype D1ZipList where
    datatypeName _ = "ZipList"
    moduleName   _ = "Control.Applicative"

instance Constructor C1_0ZipList where
    conName     _ = "ZipList"
    conIsRecord _ = True

instance Selector S1_0_0ZipList where
    selName _ = "getZipList"

-----

type Rep0U1 p = D1 D1U1 (C1 C1_0U1 U1)

instance Generic (U1 p) where
    type Rep (U1 p) = Rep0U1 p
    from U1 = M1 (M1 U1)
    to (M1 (M1 U1)) = U1

-----

type Rep0Par1 p = D1 D1Par1 (C1 C1_0Par1 (S1 S1_0_0Par1 (Rec0 p)))

instance Generic (Par1 p) where
    type Rep (Par1 p) = Rep0Par1 p
    from (Par1 p) = M1 (M1 (M1 (K1 p)))
    to (M1 (M1 (M1 (K1 p)))) = Par1 p

-----

type Rep0Rec1 f p = D1 D1Rec1 (C1 C1_0Rec1 (S1 S1_0_0Rec1 (Rec0 (f p))))

instance Generic (Rec1 f p) where
    type Rep (Rec1 f p) = Rep0Rec1 f p
    from (Rec1 r) = M1 (M1 (M1 (K1 r)))
    to (M1 (M1 (M1 (K1 r)))) = Rec1 r

-----

type Rep0K1 i c p = D1 D1K1 (C1 C1_0K1 (S1 S1_0_0K1 (Rec0 c)))

instance Generic (K1 i c p) where
    type Rep (K1 i c p) = Rep0K1 i c p
    from (K1 c) = M1 (M1 (M1 (K1 c)))
    to (M1 (M1 (M1 (K1 c)))) = K1 c

-----

type Rep0M1 i c f p = D1 D1M1 (C1 C1_0M1 (S1 S1_0_0M1 (Rec0 (f p))))

instance Generic (M1 i c f p) where
    type Rep (M1 i c f p) = Rep0M1 i c f p
    from (M1 m) = M1 (M1 (M1 (K1 m)))
    to (M1 (M1 (M1 (K1 m)))) = M1 m

-----

type Rep0ConSum f g p = D1 D1ConSum (C1 C1_0ConSum (S1 NoSelector (Rec0 (f p)))
                                 :+: C1 C1_1ConSum (S1 NoSelector (Rec0 (g p))))

instance Generic ((f :+: g) p) where
    type Rep ((f :+: g) p) = Rep0ConSum f g p

    from x = M1 (case x of
        L1 l -> L1 (M1 (M1 (K1 l)))
        R1 r -> R1 (M1 (M1 (K1 r))))

    to (M1 x) = case x of
        L1 (M1 (M1 (K1 l))) -> L1 l
        R1 (M1 (M1 (K1 r))) -> R1 r

-----

type Rep0ConProduct f g p =
    D1 D1ConProduct (C1 C1_ConProduct (S1 NoSelector (Rec0 (f p))
                                   :*: S1 NoSelector (Rec0 (g p))))

instance Generic ((f :*: g) p) where
    type Rep ((f :*: g) p) = Rep0ConProduct f g p
    from (f :*: g) = M1 (M1 (M1 (K1 f) :*: M1 (K1 g)))
    to (M1 (M1 (M1 (K1 f) :*: M1 (K1 g)))) = f :*: g

-----

type Rep0ConCompose f g p =
    D1 D1ConCompose (C1 C1_0ConCompose (S1 S1_0_0ConCompose (Rec0 (f (g p)))))

instance Generic ((f :.: g) p) where
    type Rep ((f :.: g) p) = Rep0ConCompose f g p
    from (Comp1 c) = M1 (M1 (M1 (K1 c)))
    to (M1 (M1 (M1 (K1 c)))) = Comp1 c
#endif

-----

#if !(MIN_VERSION_base(4,6,0))
type Rep1List = D1 D1List (C1 C1_0List U1 :+:
                           C1 C1_1List (S1 NoSelector Par1
                                    :*: S1 NoSelector (Rec1 [])))

instance Generic1 [] where
    type Rep1 [] = Rep1List

    from1 x = M1 (case x of
        []  -> L1 (M1 U1)
        h:t -> R1 (M1 (M1 (Par1 h) :*: M1 (Rec1 t))))

    to1 (M1 x) = case x of
        L1 (M1 U1)                            -> []
        R1 (M1 (M1 (Par1 h) :*: M1 (Rec1 t))) -> h : t

data D1List
data C1_0List
data C1_1List

instance Datatype D1List where
    datatypeName _ = "[]"
    moduleName   _ = "GHC.Types"

instance Constructor C1_0List  where
    conName _ = "[]"

instance Constructor C1_1List where
    conName   _ = ":"
    conFixity _ = Infix RightAssociative 5

-----

type Rep1Either a = D1 D1Either (C1 C1_0Either (S1 NoSelector (Rec0 a))
                             :+: C1 C1_1Either (S1 NoSelector Par1))

instance Generic1 (Either a) where
    type Rep1 (Either a) = Rep1Either a

    from1 x = M1 (case x of
        Left  l -> L1 (M1 (M1 (K1 l)))
        Right r -> R1 (M1 (M1 (Par1 r))))

    to1 (M1 x) = case x of
        L1 (M1 (M1 (K1 l)))   -> Left l
        R1 (M1 (M1 (Par1 r))) -> Right r

data D1Either
data C1_0Either
data C1_1Either

instance Datatype D1Either where
    datatypeName _ = "Either"
    moduleName   _ = "Data.Either"

instance Constructor C1_0Either where
    conName _ = "Left"

instance Constructor C1_1Either where
    conName _ = "Right"

-----

type Rep1Maybe = D1 D1Maybe (C1 C1_0Maybe U1
                         :+: C1 C1_1Maybe (S1 NoSelector Par1))

instance Generic1 Maybe where
    type Rep1 Maybe = Rep1Maybe

    from1 x = M1 (case x of
        Nothing -> L1 (M1 U1)
        Just j  -> R1 (M1 (M1 (Par1 j))))

    to1 (M1 x) = case x of
        L1 (M1 U1)            -> Nothing
        R1 (M1 (M1 (Par1 j))) -> Just j

data D1Maybe
data C1_0Maybe
data C1_1Maybe

instance Datatype D1Maybe where
    datatypeName _ = "Maybe"
    -- As of base-4.7.0.0, Maybe is actually located in GHC.Base.
    -- We don't need to worry about this for the versions of base
    -- that this instance is defined for, however.
    moduleName   _ = "Data.Maybe"

instance Constructor C1_0Maybe where
    conName _ = "Nothing"

instance Constructor C1_1Maybe where
    conName _ = "Just"

-----

type Rep1Tuple2 a = D1 D1Tuple2 (C1 C1_0Tuple2 (S1 NoSelector (Rec0 a)
                                            :*: S1 NoSelector Par1))

instance Generic1 ((,) a) where
    type Rep1 ((,) a) = Rep1Tuple2 a
    from1 (a, b) = M1 (M1 (M1 (K1 a) :*: M1 (Par1 b)))
    to1 (M1 (M1 (M1 (K1 a) :*: M1 (Par1 b)))) = (a, b)

data D1Tuple2
data C1_0Tuple2

instance Datatype D1Tuple2 where
    datatypeName _ = "(,)"
    moduleName   _ = "GHC.Tuple"

instance Constructor C1_0Tuple2 where
    conName _ = "(,)"

-----

type Rep1Tuple3 a b = D1 D1Tuple3 (C1 C1_0Tuple3 (S1 NoSelector (Rec0 a)
                                             :*: (S1 NoSelector (Rec0 b)
                                             :*:  S1 NoSelector Par1)))

instance Generic1 ((,,) a b) where
    type Rep1 ((,,) a b) = Rep1Tuple3 a b
    from1 (a, b, c) = M1 (M1 (M1 (K1 a) :*: (M1 (K1 b) :*: M1 (Par1 c))))
    to1 (M1 (M1 (M1 (K1 a) :*: (M1 (K1 b) :*: M1 (Par1 c))))) = (a, b, c)

data D1Tuple3
data C1_0Tuple3

instance Datatype D1Tuple3 where
    datatypeName _ = "(,,)"
    moduleName   _ = "GHC.Tuple"

instance Constructor C1_0Tuple3 where
    conName _ = "(,,)"

-----

type Rep1Tuple4 a b c = D1 D1Tuple4 (C1 C1_0Tuple4 ((S1 NoSelector (Rec0 a)
                                                :*:  S1 NoSelector (Rec0 b))
                                                :*: (S1 NoSelector (Rec0 c)
                                                :*:  S1 NoSelector Par1)))

instance Generic1 ((,,,) a b c) where
    type Rep1 ((,,,) a b c) = Rep1Tuple4 a b c

    from1 (a, b, c, d) = M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
                             :*: (M1 (K1 c) :*: M1 (Par1 d))))

    to1 (M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
             :*: (M1 (K1 c) :*: M1 (Par1 d)))))
      = (a, b, c, d)

data D1Tuple4
data C1_0Tuple4

instance Datatype D1Tuple4 where
    datatypeName _ = "(,,,)"
    moduleName _ = "GHC.Tuple"

instance Constructor C1_0Tuple4 where
    conName _ = "(,,,)"

-----

type Rep1Tuple5 a b c d = D1 D1Tuple5 (C1 C1_0Tuple5 ((S1 NoSelector (Rec0 a)
                                                  :*:  S1 NoSelector (Rec0 b))
                                                  :*: (S1 NoSelector (Rec0 c)
                                                  :*: (S1 NoSelector (Rec0 d)
                                                  :*:  S1 NoSelector Par1))))

instance Generic1 ((,,,,) a b c d) where
    type Rep1 ((,,,,) a b c d) = Rep1Tuple5 a b c d

    from1 (a, b, c, d, e) = M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
                                :*: (M1 (K1 c) :*: (M1 (K1 d) :*: M1 (Par1 e)))))

    to1 (M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
             :*: (M1 (K1 c) :*: (M1 (K1 d) :*: M1 (Par1 e))))))
      = (a, b, c, d, e)

data D1Tuple5
data C1_0Tuple5

instance Datatype D1Tuple5 where
    datatypeName _ = "(,,,,)"
    moduleName   _ = "GHC.Tuple"

instance Constructor C1_0Tuple5 where
    conName _ = "(,,,,)"

-----

type Rep1Tuple6 a b c d e =
    D1 D1Tuple6 (C1 C1_0Tuple6 ((S1 NoSelector (Rec0 a)
                            :*: (S1 NoSelector (Rec0 b)
                            :*:  S1 NoSelector (Rec0 c)))
                            :*: (S1 NoSelector (Rec0 d)
                            :*: (S1 NoSelector (Rec0 e)
                            :*:  S1 NoSelector Par1))))

instance Generic1 ((,,,,,) a b c d e) where
    type Rep1 ((,,,,,) a b c d e) = Rep1Tuple6 a b c d e

    from1 (a, b, c, d, e, f) = M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
                                   :*: (M1 (K1 d) :*: (M1 (K1 e) :*: M1 (Par1 f)))))

    to1 (M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
             :*: (M1 (K1 d) :*: (M1 (K1 e) :*: M1 (Par1 f))))))
      = (a, b, c, d, e, f)

data D1Tuple6
data C1_0Tuple6

instance Datatype D1Tuple6 where
    datatypeName _ = "(,,,,,)"
    moduleName   _ = "GHC.Tuple"

instance Constructor C1_0Tuple6 where
    conName _ = "(,,,,,)"

-----

type Rep1Tuple7 a b c d e f =
    D1 D1Tuple7 (C1 C1_0Tuple7 ((S1 NoSelector (Rec0 a)
                           :*:  (S1 NoSelector (Rec0 b)
                           :*:   S1 NoSelector (Rec0 c)))
                           :*: ((S1 NoSelector (Rec0 d)
                           :*:   S1 NoSelector (Rec0 e))
                           :*:  (S1 NoSelector (Rec0 f)
                           :*:   S1 NoSelector Par1))))

instance Generic1 ((,,,,,,) a b c d e f) where
    type Rep1 ((,,,,,,) a b c d e f) = Rep1Tuple7 a b c d e f

    from1 (a, b, c, d, e, f, g) = M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
                      :*: ((M1 (K1 d) :*: M1 (K1 e)) :*: (M1 (K1 f) :*: M1 (Par1 g)))))

    to1 (M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
            :*: ((M1 (K1 d) :*: M1 (K1 e)) :*: (M1 (K1 f) :*: M1 (Par1 g))))))
      = (a, b, c, d, e, f, g)

data D1Tuple7
data C1_0Tuple7

instance Datatype D1Tuple7 where
    datatypeName _ = "(,,,,,,)"
    moduleName   _ = "GHC.Tuple"

instance Constructor C1_0Tuple7 where
    conName _ = "(,,,,,,)"
#endif

-----

#if !(MIN_VERSION_base(4,4,0))
type Rep0Bool = D1 D1Bool (C1 C1_0Bool U1 :+: C1 C1_1Bool U1)

instance Generic Bool where
    type Rep Bool = Rep0Bool

    from x = M1 (case x of
        False -> L1 (M1 U1)
        True  -> R1 (M1 U1))

    to (M1 x) = case x of
        L1 (M1 U1) -> False
        R1 (M1 U1) -> True

data D1Bool
data C1_0Bool
data C1_1Bool

instance Datatype D1Bool where
    datatypeName _ = "Bool"
    moduleName _ = "GHC.Bool"

instance Constructor C1_0Bool where
    conName _ = "False"

instance Constructor C1_1Bool where
    conName _ = "True"

-----

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

-----

data D_Double
data C_Double

instance Datatype D_Double where
  datatypeName _ = "Double"
  moduleName   _ = "GHC.Float"

instance Constructor C_Double where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

type Rep0Double = D1 D_Double (C1 C_Double (S1 NoSelector (Rec0 Double)))

instance Generic Double where
  type Rep Double = Rep0Double
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x

-----

type Rep0Either a b = D1 D1Either (C1 C1_0Either (S1 NoSelector (Rec0 a))
                               :+: C1 C1_1Either (S1 NoSelector (Rec0 b)))

instance Generic (Either a b) where
    type Rep (Either a b) = Rep0Either a b

    from x = M1 (case x of
        Left  l -> L1 (M1 (M1 (K1 l)))
        Right r -> R1 (M1 (M1 (K1 r))))

    to (M1 x) = case x of
        L1 (M1 (M1 (K1 l))) -> Left l
        R1 (M1 (M1 (K1 r))) -> Right r

-----

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

-----

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

-----

type Rep0List a =
    D1 D1List (C1 C1_0List U1 :+: C1 C1_1List (S1 NoSelector (Rec0 a)
                                           :*: S1 NoSelector (Rec0 [a])))

instance Generic [a] where
    type Rep [a] = Rep0List a

    from x = M1 (case x of
        []  -> L1 (M1 U1)
        h:t -> R1 (M1 (M1 (K1 h) :*: M1 (K1 t))))

    to (M1 x) = case x of
        L1 (M1 U1)                        -> []
        R1 (M1 (M1 (K1 h) :*: M1 (K1 t))) -> h : t

-----

type Rep0Maybe a = D1 D1Maybe (C1 C1_0Maybe U1
                           :+: C1 C1_1Maybe (S1 NoSelector (Rec0 a)))

instance Generic (Maybe a) where
    type Rep (Maybe a) = Rep0Maybe a

    from x = M1 (case x of
        Nothing -> L1 (M1 U1)
        Just j  -> R1 (M1 (M1 (K1 j))))

    to (M1 x) = case x of
        L1 (M1 U1)          -> Nothing
        R1 (M1 (M1 (K1 j))) -> Just j

-----

type Rep0Ordering = D1 D1Ordering (C1 C1_0Ordering U1
                              :+: (C1 C1_1Ordering U1 :+: C1 C1_2Ordering U1))

instance Generic Ordering where
    type Rep Ordering = Rep0Ordering

    from x = M1 (case x of
        LT -> L1 (M1 U1)
        EQ -> R1 (L1 (M1 U1))
        GT -> R1 (R1 (M1 U1)))

    to (M1 x) = case x of
        L1 (M1 U1)      -> LT
        R1 (L1 (M1 U1)) -> EQ
        R1 (R1 (M1 U1)) -> GT

data D1Ordering
data C1_0Ordering
data C1_1Ordering
data C1_2Ordering

instance Datatype D1Ordering where
    datatypeName _ = "Ordering"
    moduleName _ = "GHC.Ordering"

instance Constructor C1_0Ordering where
    conName _ = "LT"

instance Constructor C1_1Ordering where
    conName _ = "EQ"

instance Constructor C1_2Ordering where
    conName _ = "GT"

-----

type Rep0Unit = D1 D1Unit (C1 C1_0Unit U1)

instance Generic () where
    type Rep () = Rep0Unit
    from () = M1 (M1 U1)
    to (M1 (M1 U1)) = ()

data D1Unit
data C1_0Unit

instance Datatype D1Unit where
    datatypeName _ = "()"
    moduleName _ = "GHC.Tuple"

instance Constructor C1_0Unit where
    conName _ = "()"

-----

type Rep0Tuple2 a b = D1 D1Tuple2 (C1 C1_0Tuple2 (S1 NoSelector (Rec0 a)
                                              :*: S1 NoSelector (Rec0 b)))

instance Generic (a, b) where
    type Rep (a, b) = Rep0Tuple2 a b
    from (a, b) = M1 (M1 (M1 (K1 a) :*: M1 (K1 b)))
    to (M1 (M1 (M1 (K1 a) :*: M1 (K1 b)))) = (a, b)

-----

type Rep0Tuple3 a b c = D1 D1Tuple3 (C1 C1_0Tuple3 (S1 NoSelector (Rec0 a)
                                               :*: (S1 NoSelector (Rec0 b)
                                               :*:  S1 NoSelector (Rec0 c))))

instance Generic (a, b, c) where
    type Rep (a, b, c) = Rep0Tuple3 a b c
    from (a, b, c) = M1 (M1 (M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c))))
    to (M1 (M1 (M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c))))) = (a, b, c)

-----

type Rep0Tuple4 a b c d = D1 D1Tuple4 (C1 C1_0Tuple4 ((S1 NoSelector (Rec0 a)
                                                  :*:  S1 NoSelector (Rec0 b))
                                                  :*: (S1 NoSelector (Rec0 c)
                                                  :*:  S1 NoSelector (Rec0 d))))

instance Generic (a, b, c, d) where
    type Rep (a, b, c, d) = Rep0Tuple4 a b c d

    from (a, b, c, d) = M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
                            :*: (M1 (K1 c) :*: M1 (K1 d))))

    to (M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
            :*: (M1 (K1 c) :*: M1 (K1 d)))))
      = (a, b, c, d)

-----

type Rep0Tuple5 a b c d e = D1 D1Tuple5 (C1 C1_0Tuple5 ((S1 NoSelector (Rec0 a)
                                                    :*:  S1 NoSelector (Rec0 b))
                                                    :*: (S1 NoSelector (Rec0 c)
                                                    :*: (S1 NoSelector (Rec0 d)
                                                    :*:  S1 NoSelector (Rec0 e)))))

instance Generic (a, b, c, d, e) where
    type Rep (a, b, c, d, e) = Rep0Tuple5 a b c d e

    from (a, b, c, d, e) = M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
                               :*: (M1 (K1 c) :*: (M1 (K1 d) :*: M1 (K1 e)))))

    to (M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
            :*: (M1 (K1 c) :*: (M1 (K1 d) :*: M1 (K1 e))))))
      = (a, b, c, d, e)

-----

type Rep0Tuple6 a b c d e f = D1 D1Tuple6 (C1 C1_0Tuple6 ((S1 NoSelector (Rec0 a)
                                                      :*: (S1 NoSelector (Rec0 b)
                                                      :*:  S1 NoSelector (Rec0 c)))
                                                      :*: (S1 NoSelector (Rec0 d)
                                                      :*: (S1 NoSelector (Rec0 e)
                                                      :*:  S1 NoSelector (Rec0 f)))))

instance Generic (a, b, c, d, e, f) where
    type Rep (a, b, c, d, e, f) = Rep0Tuple6 a b c d e f

    from (a, b, c, d, e, f) = M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
                                  :*: (M1 (K1 d) :*: (M1 (K1 e) :*: M1 (K1 f)))))

    to (M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
            :*: (M1 (K1 d) :*: (M1 (K1 e) :*: M1 (K1 f))))))
      = (a, b, c, d, e, f)

-----

type Rep0Tuple7 a b c d e f g
    = D1 D1Tuple7 (C1 C1_0Tuple7 ((S1 NoSelector (Rec0 a)
                             :*:  (S1 NoSelector (Rec0 b)
                             :*:   S1 NoSelector (Rec0 c)))
                             :*: ((S1 NoSelector (Rec0 d)
                             :*:   S1 NoSelector (Rec0 e))
                             :*:  (S1 NoSelector (Rec0 f)
                             :*:   S1 NoSelector (Rec0 g)))))

instance Generic (a, b, c, d, e, f, g) where
    type Rep (a, b, c, d, e, f, g) = Rep0Tuple7 a b c d e f g

    from (a, b, c, d, e, f, g) = M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
                     :*: ((M1 (K1 d) :*: M1 (K1 e)) :*: (M1 (K1 f) :*: M1 (K1 g)))))

    to (M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
           :*: ((M1 (K1 d) :*: M1 (K1 e)) :*: (M1 (K1 f) :*: M1 (K1 g))))))
      = (a, b, c, d, e, f, g)

#endif
