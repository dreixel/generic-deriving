
module Main where

import Data.List (intersperse)
import System.Environment (getArgs)


--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

u, tab, newline, sp :: ShowS
u = showChar '_'
tab = showString "  "
newline = showChar '\n'
sp = showChar ' '
vars :: [ShowS]
vars = map ((showChar 'x' .) . shows) [1..]
paren :: ShowS -> ShowS
paren x = showChar '(' . x . showChar ')'
concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

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

-- x is 0 or 1
pairPat, repName, rep, repInst, funs :: Int -> Int -> ShowS
pairPat x m = tuple m . sp . 
                (concatS $ intersperse sp (take (m - x) vars))

repName x m = showString "Rep" . shows x . showString "Tuple" . shows m . u

rep x m = let n    = shows m
              v    = take (m - x) vars
              vs   = concatS $ intersperse sp v
              recs = concatS $ intersperse (showString " :*: ") $ 
                       map (showString "Rec0 " .) v
              last = showString $ if (x == 1) then " :*: Par1" else ""
              body = recs . last
          in    showString "type " . repName x m . sp . vs
              . showString " = D1 Tuple" . n . showString "_  (C1 Tuple" . n 
              . showString "C_ (S1 NoSelector (" . body . showString ")))"

repInst x m = let n = shows m
                  y = shows x
                  vs = concatS $ intersperse sp (take (m - x) vars)
              in   showString "instance Representable" . y . sp
                 . paren (pairPat x m) . showString " (" . repName x m . sp
                 . vs . showString ") where"
                 . newline . funs x m

funs x m = 
  let v    = take (m - x) vars
      recs = concatS $ intersperse (showString " :*: ") $
               map (showString "K1 " .) v
      last = if (x == 1) then showString " :*: Par1 " . (vars !! (m-x))
                          else showString ""
      eq   = showChar '='
      body = paren (showString "M1 (M1 (M1 (" . recs . last . showString ")))")
      pat  = paren (pairPat 0 m)
  in tab . concatS (intersperse sp [showString "from" . shows x, pat, eq, body])
     . newline . 
     tab . concatS (intersperse sp [showString "to"   . shows x, body, eq, pat])


gen :: Int -> ShowS
gen m = concatS (intersperse (newline . newline)
          [ createDataDecls m, dataInstance m, conInstance m
          , rep 0 m, repInst 0 m, rep 1 m, repInst 1 m])

main :: IO ()
main = do let r :: [String] -> Int
              r (n:_) = read n
              r _     = error "Integer argument missing"
              com     =   showString "\n\n"
                        . concatS (map showChar (replicate 80 '-'))
                        . showString "\n\n"
          a <- getArgs
          (putStr . ($ "")) $ concatS $
            intersperse com [ gen m | m <- [2..(r a)]]
