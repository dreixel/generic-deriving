{-# LANGUAGE Rank2Types #-}

module Generics.Deriving.Data where


import Generics.Deriving.Base
import Data.Data

--------------------------------------------------------------------------------
-- Generic Data
--------------------------------------------------------------------------------
{-

 data T a b = C1 a b | C2 deriving (Typeable, Data)

 instance (Data a, Data b) => Data (T a b) where
     gfoldl k z (C1 a b) = z C1 `k` a `k` b
     gfoldl k z C2       = z C2

     gunfold k z c = case constrIndex c of
                         1 -> k (k (z C1))
                         2 -> z C2
-}

class Typeable' f where
  typeOf' :: f a -> TypeRep

class {- Typeable' f => -} Data' f where
  gfoldl'  :: (forall d b. Data d => c (d -> b) -> d -> c b)
           -> (forall g. g -> c g)
           -> f a
           -> c (f a)
{-
  gfoldl _ z = z

  gunfold :: (forall b r. Data b => c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> Constr
          -> c a

  toConstr   :: a -> Constr
  dataTypeOf  :: a -> DataType

  dataCast1 :: Typeable1 t
            => (forall d. Data d => c (t d))
            -> Maybe (c a)
  dataCast1 _ = Nothing

  dataCast2 :: Typeable2 t
            => (forall d e. (Data d, Data e) => c (t d e))
            -> Maybe (c a)
  dataCast2 _ = Nothing
-}

instance Data' U1 where
  gfoldl' k z 

