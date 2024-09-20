{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Deriving.Instances (
-- Only instances from GHC.Generics
-- and the Generic1 instances
#if !(MIN_VERSION_base(4,16,0))
    Rep0Tuple8
  , Rep0Tuple9
  , Rep0Tuple10
  , Rep0Tuple11
  , Rep0Tuple12
  , Rep0Tuple13
  , Rep0Tuple14
  , Rep0Tuple15
  , Rep1Tuple8
  , Rep1Tuple9
  , Rep1Tuple10
  , Rep1Tuple11
  , Rep1Tuple12
  , Rep1Tuple13
  , Rep1Tuple14
  , Rep1Tuple15
#endif
#if !(MIN_VERSION_base(4,14,0))
  , Rep0Kleisli
  , Rep1Kleisli
#endif
#if !(MIN_VERSION_base(4,12,0))
  , Rep0Down
  , Rep1Down
#endif
  ) where

#if !(MIN_VERSION_base(4,12,0))
import Data.Ord (Down(..))
#endif

#if !(MIN_VERSION_base(4,14,0))
import Control.Arrow (Kleisli(..))
#endif

#if !(MIN_VERSION_base(4,16,0))
import GHC.Generics
#endif

#if !(MIN_VERSION_base(4,16,0))
type Rep0Tuple8  a b c d e f g h               = Rep (a, b, c, d, e, f, g, h)
type Rep0Tuple9  a b c d e f g h i             = Rep (a, b, c, d, e, f, g, h, i)
type Rep0Tuple10 a b c d e f g h i j           = Rep (a, b, c, d, e, f, g, h, i, j)
type Rep0Tuple11 a b c d e f g h i j k         = Rep (a, b, c, d, e, f, g, h, i, j, k)
type Rep0Tuple12 a b c d e f g h i j k l       = Rep (a, b, c, d, e, f, g, h, i, j, k, l)
type Rep0Tuple13 a b c d e f g h i j k l m     = Rep (a, b, c, d, e, f, g, h, i, j, k, l, m)
type Rep0Tuple14 a b c d e f g h i j k l m n   = Rep (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
type Rep0Tuple15 a b c d e f g h i j k l m n o = Rep (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
type Rep1Tuple8  a b c d e f g               = Rep1 ((,,,,,,,) a b c d e f g)
type Rep1Tuple9  a b c d e f g h             = Rep1 ((,,,,,,,,) a b c d e f g h)
type Rep1Tuple10 a b c d e f g h i           = Rep1 ((,,,,,,,,,) a b c d e f g h i)
type Rep1Tuple11 a b c d e f g h i j         = Rep1 ((,,,,,,,,,,) a b c d e f g h i j)
type Rep1Tuple12 a b c d e f g h i j k       = Rep1 ((,,,,,,,,,,,) a b c d e f g h i j k)
type Rep1Tuple13 a b c d e f g h i j k l     = Rep1 ((,,,,,,,,,,,,) a b c d e f g h i j k l)
type Rep1Tuple14 a b c d e f g h i j k l m   = Rep1 ((,,,,,,,,,,,,,) a b c d e f g h i j k l m)
type Rep1Tuple15 a b c d e f g h i j k l m n = Rep1 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n)
deriving instance Generic (a, b, c, d, e, f, g, h)
deriving instance Generic (a, b, c, d, e, f, g, h, i)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
deriving instance Generic1 ((,,,,,,,) a b c d e f g)
deriving instance Generic1 ((,,,,,,,,) a b c d e f g h)
deriving instance Generic1 ((,,,,,,,,,) a b c d e f g h i)
deriving instance Generic1 ((,,,,,,,,,,) a b c d e f g h i j)
deriving instance Generic1 ((,,,,,,,,,,,) a b c d e f g h i j k)
deriving instance Generic1 ((,,,,,,,,,,,,) a b c d e f g h i j k l)
deriving instance Generic1 ((,,,,,,,,,,,,,) a b c d e f g h i j k l m)
deriving instance Generic1 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n)
#endif

#if !(MIN_VERSION_base(4,14,0))
type Rep0Kleisli m a b = Rep  (Kleisli m a b)
type Rep1Kleisli m a   = Rep1 (Kleisli m a)
deriving instance Generic  (Kleisli m a b)
deriving instance Generic1 (Kleisli m a)
#endif

#if !(MIN_VERSION_base(4,12,0))
type Rep0Down a = Rep (Down a)
type Rep1Down   = Rep1 Down
deriving instance Generic (Down a)
deriving instance Generic1 Down
#endif
