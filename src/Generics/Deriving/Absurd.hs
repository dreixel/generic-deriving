{-# language CPP #-}
#if __GLASGOW_HASKELL__ >= 708
{-# language EmptyCase #-}
#endif
module Generics.Deriving.Absurd where
import GHC.Generics
#if __GLASGOW_HASKELL__ < 708
import GHC.Conc (pseq)
#endif

absurd1 :: V1 a -> b
absurd1 x =
#if __GLASGOW_HASKELL__ >= 708
  case x of {}
#else
  -- Using pseq instead of seq or a bang pattern guarantees
  -- that the impossible error will never be reached.
  pseq x impossible
#endif
{-# INLINE absurd1 #-}

impossible :: a
impossible = error "Utterly impossible"
{-# NOINLINE impossible #-}
