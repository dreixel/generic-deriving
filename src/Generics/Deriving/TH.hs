{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -w           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Deriving.TH
-- Copyright   :  (c) 2008--2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains Template Haskell code that can be used to
-- automatically generate the boilerplate code for the generic deriving
-- library. For now, it generates only the 'Generic' instance.
-- Empty datatypes are not yet supported.
-----------------------------------------------------------------------------

-- Adapted from Generics.Regular.TH
module Generics.Deriving.TH (
      
      deriveMeta
    , deriveData
    , deriveConstructors
    , deriveSelectors

#if __GLASGOW_HASKELL__ < 701
    , deriveAll
    , deriveRepresentable0
    , deriveRep0
    , simplInstance
#endif
  ) where

import Generics.Deriving.Base

import Language.Haskell.TH hiding (Fixity())
import Language.Haskell.TH.Syntax (Lift(..))

import Data.List (intercalate)
import Control.Monad

-- | Given the names of a generic class, a type to instantiate, a function in
-- the class and the default implementation, generates the code for a basic
-- generic instance.
simplInstance :: Name -> Name -> Name -> Name -> Q [Dec]
simplInstance cl ty fn df = do
  i <- reify (genRepName 0 ty)
  x <- newName "x"
  let typ = ForallT [PlainTV x] [] 
        ((foldl (\a -> AppT a . VarT . tyVarBndrToName) (ConT (genRepName 0 ty)) 
          (typeVariables i)) `AppT` (VarT x))
  fmap (: []) $ instanceD (cxt []) (conT cl `appT` conT ty)
    [funD fn [clause [] (normalB (varE df `appE` 
      (sigE (global 'undefined) (return typ)))) []]]


-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, the 'Selector'
-- instances, and the 'Representable0' instance.
deriveAll :: Name -> Q [Dec]
deriveAll n =
  do a <- deriveMeta n
     b <- deriveRepresentable0 n
     return (a ++ b)

-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, and the 'Selector'
-- instances.
deriveMeta :: Name -> Q [Dec]
deriveMeta n =
  do a <- deriveData n
     b <- deriveConstructors n
     c <- deriveSelectors n
     return (a ++ b ++ c)

-- | Given a datatype name, derive a datatype and instance of class 'Datatype'.
deriveData :: Name -> Q [Dec]
deriveData = dataInstance

-- | Given a datatype name, derive datatypes and 
-- instances of class 'Constructor'.
deriveConstructors :: Name -> Q [Dec]
deriveConstructors = constrInstance

-- | Given a datatype name, derive datatypes and instances of class 'Selector'.
deriveSelectors :: Name -> Q [Dec]
deriveSelectors = selectInstance

-- | Given the type and the name (as string) for the Representable0 type
-- synonym to derive, generate the 'Representable0' instance.
deriveRepresentable0 :: Name -> Q [Dec]
deriveRepresentable0 n = do
    rep0 <- deriveRep0 n
    inst <- deriveInst n
    return $ rep0 ++ inst

-- | Derive only the 'Rep0' type synonym. Not needed if 'deriveRepresentable0'
-- is used.
deriveRep0 :: Name -> Q [Dec]
deriveRep0 n = do
  i <- reify n
  fmap (:[]) $ tySynD (genRepName 0 n) (typeVariables i) (rep0Type n)

deriveInst :: Name -> Q [Dec]
deriveInst t = do
  i <- reify t
  let typ q = foldl (\a -> AppT a . VarT . tyVarBndrToName) (ConT q) 
                (typeVariables i)
#if __GLASGOW_HASKELL__ >= 707
  let tyIns = TySynInstD ''Rep (TySynEqn [typ t] (typ (genRepName 0 t)))
#else
  let tyIns = TySynInstD ''Rep [typ t] (typ (genRepName 0 t))
#endif
  fcs <- mkFrom t 1 0 t
  tcs <- mkTo   t 1 0 t
  liftM (:[]) $
    instanceD (cxt []) (conT ''Generic `appT` return (typ t))
                         [return tyIns, funD 'from fcs, funD 'to tcs]


dataInstance :: Name -> Q [Dec]
dataInstance n = do
  i <- reify n
  case i of
    TyConI (DataD    _ n _ _ _) -> mkInstance n
    TyConI (NewtypeD _ n _ _ _) -> mkInstance n
    _ -> return []
  where
    mkInstance n = do
      ds <- mkDataData n
      is <- mkDataInstance n
      return $ [ds,is]

constrInstance :: Name -> Q [Dec]
constrInstance n = do
  i <- reify n
  case i of
    TyConI (DataD    _ n _ cs _) -> mkInstance n cs
    TyConI (NewtypeD _ n _ c  _) -> mkInstance n [c]
    _ -> return []
  where
    mkInstance n cs = do
      ds <- mapM (mkConstrData n) cs
      is <- mapM (mkConstrInstance n) cs
      return $ ds ++ is

selectInstance :: Name -> Q [Dec]
selectInstance n = do
  i <- reify n
  case i of
    TyConI (DataD    _ n _ cs _) -> mkInstance n cs
    TyConI (NewtypeD _ n _ c  _) -> mkInstance n [c]
    _ -> return []
  where
    mkInstance n cs = do
      ds <- mapM (mkSelectData n) cs
      is <- mapM (mkSelectInstance n) cs
      return $ concat (ds ++ is)

typeVariables :: Info -> [TyVarBndr]
typeVariables (TyConI (DataD    _ _ tv _ _)) = tv
typeVariables (TyConI (NewtypeD _ _ tv _ _)) = tv
typeVariables _                           = []

tyVarBndrToName :: TyVarBndr -> Name
tyVarBndrToName (PlainTV  name)   = name
tyVarBndrToName (KindedTV name _) = name

stripRecordNames :: Con -> Con
stripRecordNames (RecC n f) =
  NormalC n (map (\(_, s, t) -> (s, t)) f)
stripRecordNames c = c

genName :: [Name] -> Name
genName = mkName . (++"_") . intercalate "_" . map nameBase

genRepName :: Int -> Name -> Name
genRepName n = mkName . (++"_") . (("Rep" ++ show n) ++) . nameBase

mkDataData :: Name -> Q Dec
mkDataData n = dataD (cxt []) (genName [n]) [] [] []

mkConstrData :: Name -> Con -> Q Dec
mkConstrData dt (NormalC n _) =
  dataD (cxt []) (genName [dt, n]) [] [] [] 
mkConstrData dt r@(RecC _ _) =
  mkConstrData dt (stripRecordNames r)
mkConstrData dt (InfixC t1 n t2) =
  mkConstrData dt (NormalC n [t1,t2])

mkSelectData :: Name -> Con -> Q [Dec]
mkSelectData dt r@(RecC n fs) = return (map one fs)
  where one (f, _, _) = DataD [] (genName [dt, n, f]) [] [] []
mkSelectData dt _ = return []


mkDataInstance :: Name -> Q Dec
mkDataInstance n =
  instanceD (cxt []) (appT (conT ''Datatype) (conT $ genName [n]))
    [funD 'datatypeName [clause [wildP] (normalB (stringE (nameBase n))) []]
    ,funD 'moduleName   [clause [wildP] (normalB (stringE name)) []]]
  where
    name = maybe (error "Cannot fetch module name!") id (nameModule n)

instance Lift Fixity where
  lift Prefix      = conE 'Prefix
  lift (Infix a n) = conE 'Infix `appE` [| a |] `appE` [| n |]

instance Lift Associativity where
  lift LeftAssociative  = conE 'LeftAssociative
  lift RightAssociative = conE 'RightAssociative
  lift NotAssociative   = conE 'NotAssociative

mkConstrInstance :: Name -> Con -> Q Dec
mkConstrInstance dt (NormalC n _) = mkConstrInstanceWith dt n []
mkConstrInstance dt (RecC    n _) = mkConstrInstanceWith dt n
      [ funD 'conIsRecord [clause [wildP] (normalB (conE 'True)) []]]
mkConstrInstance dt (InfixC t1 n t2) =
    do
      i <- reify n
      let fi = case i of
                 DataConI _ _ _ f -> convertFixity f
                 _ -> Prefix
      instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName [dt, n]))
        [funD 'conName   [clause [wildP] (normalB (stringE (nameBase n))) []],
         funD 'conFixity [clause [wildP] (normalB [| fi |]) []]]
  where
    convertFixity (Fixity n d) = Infix (convertDirection d) n
    convertDirection InfixL = LeftAssociative
    convertDirection InfixR = RightAssociative
    convertDirection InfixN = NotAssociative

mkConstrInstanceWith :: Name -> Name -> [Q Dec] -> Q Dec
mkConstrInstanceWith dt n extra = 
  instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName [dt, n]))
    (funD 'conName [clause [wildP] (normalB (stringE (nameBase n))) []] : extra)

mkSelectInstance :: Name -> Con -> Q [Dec]
mkSelectInstance dt r@(RecC n fs) = return (map one fs) where
  one (f, _, _) = 
    InstanceD ([]) (AppT (ConT ''Selector) (ConT $ genName [dt, n, f]))
      [FunD 'selName [Clause [WildP] 
        (NormalB (LitE (StringL (nameBase f)))) []]]
mkSelectInstance _ _ = return []

rep0Type :: Name -> Q Type
rep0Type n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  (conT ''D1) `appT` (conT $ genName [dt]) `appT` 
                    (foldr1' sum (conT ''V1) 
                      (map (rep0Con (dt, map tyVarBndrToName vs)) cs))
                TyConI (NewtypeD _ dt vs c _) ->
                  (conT ''D1) `appT` (conT $ genName [dt]) `appT`
                    (rep0Con (dt, map tyVarBndrToName vs) c)
                TyConI (TySynD t _ _) -> error "type synonym?" 
                _ -> error "unknown construct" 
      --appT b (conT $ mkName (nameBase n))
      b where
    sum :: Q Type -> Q Type -> Q Type
    sum a b = conT ''(:+:) `appT` a `appT` b


rep0Con :: (Name, [Name]) -> Con -> Q Type
rep0Con (dt, vs) (NormalC n []) =
    conT ''C1 `appT` (conT $ genName [dt, n]) `appT` 
     (conT ''S1 `appT` conT ''NoSelector `appT` conT ''U1)
rep0Con (dt, vs) (NormalC n fs) =
    conT ''C1 `appT` (conT $ genName [dt, n]) `appT` 
     (foldr1 prod (map (repField (dt, vs) . snd) fs)) where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b
rep0Con (dt, vs) r@(RecC n []) =
    conT ''C1 `appT` (conT $ genName [dt, n]) `appT` conT ''U1
rep0Con (dt, vs) r@(RecC n fs) =
    conT ''C1 `appT` (conT $ genName [dt, n]) `appT` 
      (foldr1 prod (map (repField' (dt, vs) n) fs)) where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b

rep0Con d (InfixC t1 n t2) = rep0Con d (NormalC n [t1,t2])

--dataDeclToType :: (Name, [Name]) -> Type
--dataDeclToType (dt, vs) = foldl (\a b -> AppT a (VarT b)) (ConT dt) vs

repField :: (Name, [Name]) -> Type -> Q Type
--repField d t | t == dataDeclToType d = conT ''I
repField d t = conT ''S1 `appT` conT ''NoSelector `appT`
                 (conT ''Rec0 `appT` return t)

repField' :: (Name, [Name]) -> Name -> (Name, Strict, Type) -> Q Type
--repField' d ns (_, _, t) | t == dataDeclToType d = conT ''I
repField' (dt, vs) ns (f, _, t) = conT ''S1 `appT` conT (genName [dt, ns, f]) 
                                    `appT` (conT ''Rec0 `appT` return t)
-- Note: we should generate Par0 too, at some point


mkFrom :: Name -> Int -> Int -> Name -> Q [Q Clause]
mkFrom ns m i n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      let wrapE e = lrE m i e
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  zipWith (fromCon wrapE ns (dt, map tyVarBndrToName vs)
                    (length cs)) [0..] cs
                TyConI (NewtypeD _ dt vs c _) ->
                  [fromCon wrapE ns (dt, map tyVarBndrToName vs) 1 0 c]
                TyConI (TySynD t _ _) -> error "type synonym?" 
                  -- [clause [varP (field 0)] (normalB (wrapE $ conE 'K1 `appE` varE (field 0))) []]
                _ -> error "unknown construct"
      return b

mkTo :: Name -> Int -> Int -> Name -> Q [Q Clause]
mkTo ns m i n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      let wrapP p = lrP m i p
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  zipWith (toCon wrapP ns (dt, map tyVarBndrToName vs)
                    (length cs)) [0..] cs
                TyConI (NewtypeD _ dt vs c _) ->
                  [toCon wrapP ns (dt, map tyVarBndrToName vs) 1 0 c]
                TyConI (TySynD t _ _) -> error "type synonym?" 
                  -- [clause [wrapP $ conP 'K1 [varP (field 0)]] (normalB $ varE (field 0)) []]
                _ -> error "unknown construct" 
      return b

fromCon :: (Q Exp -> Q Exp) -> Name -> (Name, [Name]) -> Int -> Int -> Con -> Q Clause
fromCon wrap ns (dt, vs) m i (NormalC cn []) =
  clause
    [conP cn []]
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ appE (conE 'M1) $ 
      conE 'M1 `appE` (conE 'U1)) []
fromCon wrap ns (dt, vs) m i (NormalC cn fs) =
  -- runIO (putStrLn ("constructor " ++ show ix)) >>
  clause
    [conP cn (map (varP . field) [0..length fs - 1])]
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ conE 'M1 `appE` 
      foldr1 prod (zipWith (fromField (dt, vs)) [0..] (map snd fs))) []
  where prod x y = conE '(:*:) `appE` x `appE` y
fromCon wrap ns (dt, vs) m i r@(RecC cn []) =
  clause
    [conP cn []]
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ conE 'M1 `appE` (conE 'U1)) []
fromCon wrap ns (dt, vs) m i r@(RecC cn fs) =
  clause
    [conP cn (map (varP . field) [0..length fs - 1])]
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ conE 'M1 `appE` 
      foldr1 prod (zipWith (fromField (dt, vs)) [0..] (map trd fs))) []
  where prod x y = conE '(:*:) `appE` x `appE` y
fromCon wrap ns (dt, vs) m i (InfixC t1 cn t2) =
  fromCon wrap ns (dt, vs) m i (NormalC cn [t1,t2])

fromField :: (Name, [Name]) -> Int -> Type -> Q Exp
--fromField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conE 'I `appE` varE (field nr)
fromField (dt, vs) nr t = conE 'M1 `appE` (conE 'K1 `appE` varE (field nr))

toCon :: (Q Pat -> Q Pat) -> Name -> (Name, [Name]) -> Int -> Int -> Con -> Q Clause
toCon wrap ns (dt, vs) m i (NormalC cn []) =
    clause
      [wrap $ conP 'M1 [lrP m i $ conP 'M1 [conP 'M1 [conP 'U1 []]]]]
      (normalB $ conE cn) []
toCon wrap ns (dt, vs) m i (NormalC cn fs) =
    -- runIO (putStrLn ("constructor " ++ show ix)) >>
    clause
      [wrap $ conP 'M1 [lrP m i $ conP 'M1
        [foldr1 prod (zipWith (toField (dt, vs)) [0..] (map snd fs))]]]
      (normalB $ foldl appE (conE cn) (map (varE . field) [0..length fs - 1])) []
  where prod x y = conP '(:*:) [x,y]
toCon wrap ns (dt, vs) m i r@(RecC cn []) =
    clause
      [wrap $ conP 'M1 [lrP m i $ conP 'M1 [conP 'U1 []]]]
      (normalB $ conE cn) []
toCon wrap ns (dt, vs) m i r@(RecC cn fs) =
    clause
      [wrap $ conP 'M1 [lrP m i $ conP 'M1
        [foldr1 prod (zipWith (toField (dt, vs)) [0..] (map trd fs))]]]
      (normalB $ foldl appE (conE cn) (map (varE . field) [0..length fs - 1])) []
  where prod x y = conP '(:*:) [x,y]
toCon wrap ns (dt, vs) m i (InfixC t1 cn t2) =
  toCon wrap ns (dt, vs) m i (NormalC cn [t1,t2])

toField :: (Name, [Name]) -> Int -> Type -> Q Pat
--toField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conP 'I [varP (field nr)]
toField (dt, vs) nr t = conP 'M1 [conP 'K1 [varP (field nr)]]


field :: Int -> Name
field n = mkName $ "f" ++ show n

lrP :: Int -> Int -> (Q Pat -> Q Pat)
lrP 1 0 p = p
lrP m 0 p = conP 'L1 [p]
lrP m i p = conP 'R1 [lrP (m-1) (i-1) p]

lrE :: Int -> Int -> (Q Exp -> Q Exp)
lrE 1 0 e = e
lrE m 0 e = conE 'L1 `appE` e
lrE m i e = conE 'R1 `appE` lrE (m-1) (i-1) e

trd (_,_,c) = c

-- | Variant of foldr1 which returns a special element for empty lists
foldr1' f x [] = x
foldr1' _ _ [x] = x
foldr1' f x (h:t) = f h (foldr1' f x t)
