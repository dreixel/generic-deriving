
module Main where


main = undefined

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------
{-
data (,) a b = (,) a b

data Tuple2_
data Tuple2C_

instance Datatype Tuple2_ where
  datatypeName _ = "(,)"
  moduleName   _ = "Prelude"

instance Constructor Tuple2C_ where conName _ = "(,)"


type Rep0Tuple2_ a b = D1 Tuple2_ (C1 Tuple2C_ (S1 NoSelector (
                         Rec0 a :*: Rec0 b)))
instance Representable0 ((,) a b) (Rep0Tuple2_ a b) where  
  from0 (a,b) = M1 (M1 (M1 (K1 a :*: K1 b)))
  to0 (M1 (M1 (M1 (K1 a :*: K1 b)))) = (,) a b

type Rep1Tuple2_ a = D1 Tuple2_ (C1 Tuple2C_ (S1 NoSelector (
                       Rec0 a :*: Par1)))
instance Representable1 ((,) a) (Rep1Tuple2_ a) where  
  from1 (a,b) = M1 (M1 (M1 (K1 a :*: Par1 b)))
  to1 (M1 (M1 (M1 (K1 a :*: Par1 b)))) = (,) a b
-}
u, tab, newline :: ShowS
u = showChar '_'
tab = showString "  "
newline = showChar '\n'

tuple :: Int -> ShowS
tuple m = showChar '(' . showString (replicate (m-1) ',') . showChar ')'

unlinesS :: [ShowS] -> ShowS
unlinesS = foldr1 (\a b -> a . newline . b)

createDataDecls :: Int -> ShowS
createDataDecls m = let n = shows m
                        s = showString "data Tuple"
                    in s . n . u . newline . s . n . showChar 'C' . u

dataInstance :: Int -> ShowS
dataInstance m = let n = shows m
                     l1 =   showString "instance Datatype Tuple" 
                          . n . u . showString " where"
                     l2 =   tab . showString "datatypeName _ = \"" 
                          . tuple m . showChar '"'
                     l3 = tab . showString "moduleName   _ = \"Prelude\""
                 in unlinesS [l1, l2, l3]

conInstance :: Int -> ShowS
conInstance m = let n = shows m
                in   showString "instance Constructor Tuple" . n . u
                   . showString " where conName _ = \"" . tuple m . showChar '"'
