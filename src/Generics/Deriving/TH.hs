{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
-----------------------------------------------------------------------------

-- Adapted from Generics.Regular.TH
module Generics.Deriving.TH (

      deriveMeta
    , deriveData
    , deriveConstructors
    , deriveSelectors

    , deriveAll
    , deriveAll1
    , deriveAll0And1
    , deriveRepresentable0
    , deriveRepresentable1
    , deriveRep0
    , deriveRep1
    , simplInstance
  ) where

import Generics.Deriving.Base

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (Lift(..))
import Language.Haskell.TH hiding (Fixity())

import Data.Char (isAlphaNum, ord)
import Data.Function (on)
import Data.List (foldl', intercalate)
import Data.Map as Map (Map, fromList, lookup)
import Data.Maybe (fromMaybe)
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
      (sigE (varE 'undefined) (return typ)))) []]]


-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, the 'Selector'
-- instances, and the 'Representable0' instance.
deriveAll :: Name -> Q [Dec]
deriveAll n =
  do a <- deriveMeta n
     b <- deriveRepresentable0 n
     return (a ++ b)

-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, the 'Selector'
-- instances, and the 'Representable1' instance.
deriveAll1 :: Name -> Q [Dec]
deriveAll1 n =
  do a <- deriveMeta n
     b <- deriveRepresentable1 n
     return (a ++ b)

-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, the 'Selector'
-- instances, the 'Representable0' instance, and the 'Representable1' instance.
deriveAll0And1 :: Name -> Q [Dec]
deriveAll0And1 n =
  do a <- deriveMeta n
     b <- deriveRepresentable0 n
     c <- deriveRepresentable1 n
     return (a ++ b ++ c)

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

-- | Given the type and the name (as string) for the Representable1 type
-- synonym to derive, generate the 'Representable1' instance.
deriveRepresentable1 :: Name -> Q [Dec]
deriveRepresentable1 n = do
    rep1  <- deriveRep1 n
    inst1 <- deriveInst1 n
    return $ rep1 ++ inst1

-- | Derive only the 'Rep0' type synonym. Not needed if 'deriveRepresentable0'
-- is used.
deriveRep0 :: Name -> Q [Dec]
deriveRep0 n = do
  i <- reify n
  fmap (:[]) $ tySynD (genRepName 0 n) (typeVariables i) (repType Gen0 n)

-- | Derive only the 'Rep1' type synonym. Not needed if 'deriveRepresentable1'
-- is used.
deriveRep1 :: Name -> Q [Dec]
deriveRep1 n = do
  i <- reify n
  let tvbs = typeVariables i

  let (lhsTvbs, rhsNb) = case splitAt (length tvbs - 1) tvbs of
        (lhsTvbs', rhsTvb':_) | canRealizeKindStar (tyVarBndrToKind rhsTvb')
                              -> (lhsTvbs', NameBase $ tyVarBndrToName rhsTvb')
        _ -> kindError

  fmap (:[]) $ tySynD (genRepName 1 n)
                      lhsTvbs
                      (repType (Gen1 rhsNb) n)

deriveInst :: Name -> Q [Dec]
deriveInst t = do
  i <- reify t
  let typ q = foldl (\a -> AppT a . VarT . tyVarBndrToName) (ConT q)
                (typeVariables i)
  deriveInstCommon Gen0 ''Generic ''Rep 0 'from 'to typ t

deriveInst1 :: Name -> Q [Dec]
deriveInst1 t = do
  i <- reify t
  let tvbs = typeVariables i

  let (lhsTvbs, rhsNb) = case splitAt (length tvbs - 1) tvbs of
          (lhsTvbs', rhsTvb':_) | canRealizeKindStar (tyVarBndrToKind rhsTvb')
                                -> (lhsTvbs', NameBase $ tyVarBndrToName rhsTvb')
          _ -> kindError

  let typ q = foldl (\a -> AppT a . VarT . tyVarBndrToName) (ConT q) lhsTvbs
  deriveInstCommon (Gen1 rhsNb) ''Generic1 ''Rep1 1 'from1 'to1 typ t

deriveInstCommon :: GenericKind -> Name -> Name -> Int -> Name -> Name
                 -> (Name -> Type) -> Name -> Q [Dec]
deriveInstCommon gk genericName repName n fromName toName typ t = do
#if __GLASGOW_HASKELL__ >= 707
  let tyIns = TySynInstD repName (TySynEqn [typ t] (typ (genRepName n t)))
#else
  let tyIns = TySynInstD repName [typ t] (typ (genRepName n t))
#endif
  fcs <- mkFrom gk t 1 0 t
  tcs <- mkTo   gk t 1 0 t
  liftM (:[]) $
    instanceD (cxt []) (conT genericName `appT` return (typ t))
                         [return tyIns, funD fromName fcs, funD toName tcs]

dataInstance :: Name -> Q [Dec]
dataInstance n = do
  i <- reify n
  case i of
    TyConI (DataD    _ n' _ _ _) -> mkInstance n'
    TyConI (NewtypeD _ n' _ _ _) -> mkInstance n'
    _ -> return []
  where
    mkInstance n' = do
      ds <- mkDataData n'
      is <- mkDataInstance n'
      return $ [ds,is]

constrInstance :: Name -> Q [Dec]
constrInstance n = do
  i <- reify n
  case i of
    TyConI (DataD    _ n' _ cs _) -> mkInstance n' cs
    TyConI (NewtypeD _ n' _ c  _) -> mkInstance n' [c]
    _ -> return []
  where
    mkInstance n' cs = do
      ds <- mapM (mkConstrData n') cs
      is <- mapM (mkConstrInstance n') cs
      return $ ds ++ is

selectInstance :: Name -> Q [Dec]
selectInstance n = do
  i <- reify n
  case i of
    TyConI (DataD    _ n' _ cs _) -> mkInstance n' cs
    TyConI (NewtypeD _ n' _ c  _) -> mkInstance n' [c]
    _ -> return []
  where
    mkInstance n' cs = do
      ds <- mapM (mkSelectData n') cs
      is <- mapM (mkSelectInstance n') cs
      return $ concat (ds ++ is)

typeVariables :: Info -> [TyVarBndr]
typeVariables (TyConI (DataD    _ _ tv _ _)) = tv
typeVariables (TyConI (NewtypeD _ _ tv _ _)) = tv
typeVariables _                           = []

tyVarBndrToName :: TyVarBndr -> Name
tyVarBndrToName (PlainTV  name)   = name
tyVarBndrToName (KindedTV name _) = name

tyVarBndrToKind :: TyVarBndr -> Kind
tyVarBndrToKind (PlainTV  _)   = starK
tyVarBndrToKind (KindedTV _ k) = k

stripRecordNames :: Con -> Con
stripRecordNames (RecC n f) =
  NormalC n (map (\(_, s, t) -> (s, t)) f)
stripRecordNames c = c

genName :: [Name] -> Name
genName = mkName . (++"_") . intercalate "_" . map (sanitizeName . show)

genRepName :: Int -> Name -> Name
genRepName n = mkName . (++"_") . (("Rep" ++ show n) ++) . sanitizeName . show

mkDataData :: Name -> Q Dec
mkDataData n = dataD (cxt []) (genName [n]) [] [] []

mkConstrData :: Name -> Con -> Q Dec
mkConstrData dt (NormalC n _) =
  dataD (cxt []) (genName [dt, n]) [] [] []
mkConstrData dt r@(RecC _ _) =
  mkConstrData dt (stripRecordNames r)
mkConstrData dt (InfixC t1 n t2) =
  mkConstrData dt (NormalC n [t1,t2])
mkConstrData _ (ForallC _ _ con) = forallCError con

mkSelectData :: Name -> Con -> Q [Dec]
mkSelectData dt (RecC n fs) = return (map one fs)
  where one (f, _, _) = DataD [] (genName [dt, n, f]) [] [] []
mkSelectData _ _ = return []


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
mkConstrInstance dt (InfixC _ n _) =
    do
      i <- reify n
#if __GLASGOW_HASKELL__ >= 711
      fi <- case i of
                 DataConI{} -> fmap convertFixity $ reifyFixity n
                 _ -> return Prefix
#else
      let fi = case i of
                 DataConI _ _ _ f -> convertFixity f
                 _ -> Prefix
#endif
      instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName [dt, n]))
        [funD 'conName   [clause [wildP] (normalB (stringE (nameBase n))) []],
         funD 'conFixity [clause [wildP] (normalB [| fi |]) []]]
  where
    convertFixity (Fixity n' d) = Infix (convertDirection d) n'
    convertDirection InfixL = LeftAssociative
    convertDirection InfixR = RightAssociative
    convertDirection InfixN = NotAssociative
mkConstrInstance _ (ForallC _ _ con) = forallCError con

mkConstrInstanceWith :: Name -> Name -> [Q Dec] -> Q Dec
mkConstrInstanceWith dt n extra =
  instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName [dt, n]))
    (funD 'conName [clause [wildP] (normalB (stringE (nameBase n))) []] : extra)

mkSelectInstance :: Name -> Con -> Q [Dec]
mkSelectInstance dt (RecC n fs) = return (map one fs) where
  one (f, _, _) =
    InstanceD ([]) (AppT (ConT ''Selector) (ConT $ genName [dt, n, f]))
      [FunD 'selName [Clause [WildP]
        (NormalB (LitE (StringL (nameBase f)))) []]]
mkSelectInstance _ _ = return []

repType :: GenericKind -> Name -> Q Type
repType gk n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  (conT ''D1) `appT` (conT $ genName [dt]) `appT`
                    (foldr1' sum' (conT ''V1)
                      (map (repCon gk (dt, map tyVarBndrToName vs)) cs))
                TyConI (NewtypeD _ dt vs c _) ->
                  (conT ''D1) `appT` (conT $ genName [dt]) `appT`
                    (repCon gk (dt, map tyVarBndrToName vs) c)
                TyConI (TySynD _ _ _) -> error "type synonym?"
                _ -> error "unknown construct"
      --appT b (conT $ mkName (nameBase n))
      b where
    sum' :: Q Type -> Q Type -> Q Type
    sum' a b = conT ''(:+:) `appT` a `appT` b


repCon :: GenericKind -> (Name, [Name]) -> Con -> Q Type
repCon _ (dt, _) (NormalC n []) =
    conT ''C1 `appT` (conT $ genName [dt, n]) `appT`
     (conT ''S1 `appT` conT ''NoSelector `appT` conT ''U1)
repCon gk (dt, vs) (NormalC n fs) =
    conT ''C1 `appT` (conT $ genName [dt, n]) `appT`
     (foldr1 prod (map (repField gk (dt, vs) . snd) fs)) where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b
repCon _ (dt, _) (RecC n []) =
    conT ''C1 `appT` (conT $ genName [dt, n]) `appT` conT ''U1
repCon gk (dt, vs) (RecC n fs) =
    conT ''C1 `appT` (conT $ genName [dt, n]) `appT`
      (foldr1 prod (map (repField' gk (dt, vs) n) fs)) where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b

repCon gk d (InfixC t1 n t2) = repCon gk d (NormalC n [t1,t2])
repCon _ _ (ForallC _ _ con) = forallCError con

--dataDeclToType :: (Name, [Name]) -> Type
--dataDeclToType (dt, vs) = foldl (\a b -> AppT a (VarT b)) (ConT dt) vs

repField :: GenericKind -> (Name, [Name]) -> Type -> Q Type
--repField d t | t == dataDeclToType d = conT ''I
repField gk _ t = conT ''S1 `appT` conT ''NoSelector `appT`
                   (repFieldArg gk =<< expandSyn t)

repField' :: GenericKind -> (Name, [Name]) -> Name -> (Name, Strict, Type) -> Q Type
--repField' d ns (_, _, t) | t == dataDeclToType d = conT ''I
repField' gk (dt, _) ns (f, _, t) = conT ''S1 `appT` conT (genName [dt, ns, f]) `appT`
                                     (repFieldArg gk =<< expandSyn t)
-- Note: we should generate Par0 too, at some point

repFieldArg :: GenericKind -> Type -> Q Type
repFieldArg _ ForallT{} = rankNError
repFieldArg gk (SigT t _) = repFieldArg gk t
repFieldArg Gen0 t = conT ''Rec0 `appT` return t
repFieldArg (Gen1 nb) (VarT t) | NameBase t == nb = conT ''Par1
repFieldArg gk@(Gen1 nb) t =
  let tyHead:tyArgs      = unapplyTy t
      numLastArgs        = min 1 $ length tyArgs
      (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs
      rec0Type           = conT ''Rec0  `appT` return t
      phiType            = return $ applyTy tyHead lhsArgs

      inspectTy :: Type -> Q Type
      inspectTy (VarT a)
        | NameBase a == nb
        = conT ''Rec1 `appT` phiType
      inspectTy (SigT ty _) = inspectTy ty
      inspectTy beta
        | not (ground beta nb)
        = conT ''(:.:) `appT` phiType
                       `appT` repFieldArg gk beta
      inspectTy _ = rec0Type

   in if any (not . (`ground` nb)) lhsArgs
         then outOfPlaceTyVarError
         else case rhsArgs of
              []   -> rec0Type
              ty:_ -> inspectTy ty

mkFrom :: GenericKind -> Name -> Int -> Int -> Name -> Q [Q Clause]
mkFrom gk ns m i n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      let wrapE e = lrE m i e
      i' <- reify n
      let b = case i' of
                TyConI (DataD _ dt vs cs _) ->
                  if null cs
                     then [errorClauseFrom dt]
                     else zipWith (fromCon gk wrapE ns (dt, map tyVarBndrToName vs)
                            (length cs)) [0..] cs
                TyConI (NewtypeD _ dt vs c _) ->
                  [fromCon gk wrapE ns (dt, map tyVarBndrToName vs) 1 0 c]
                TyConI (TySynD _ _ _) -> error "type synonym?"
                  -- [clause [varP (field 0)] (normalB (wrapE $ conE 'K1 `appE` varE (field 0))) []]
                _ -> error "unknown construct"
      return b

mkTo :: GenericKind -> Name -> Int -> Int -> Name -> Q [Q Clause]
mkTo gk ns m i n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      let wrapP p = lrP m i p
      i' <- reify n
      let b = case i' of
                TyConI (DataD _ dt vs cs _) ->
                  if null cs
                     then [errorClauseTo dt]
                     else zipWith (toCon gk wrapP ns (dt, map tyVarBndrToName vs)
                            (length cs)) [0..] cs
                TyConI (NewtypeD _ dt vs c _) ->
                  [toCon gk wrapP ns (dt, map tyVarBndrToName vs) 1 0 c]
                TyConI (TySynD _ _ _) -> error "type synonym?"
                  -- [clause [wrapP $ conP 'K1 [varP (field 0)]] (normalB $ varE (field 0)) []]
                _ -> error "unknown construct"
      return b

fromCon :: GenericKind -> (Q Exp -> Q Exp) -> Name -> (Name, [Name]) -> Int -> Int -> Con -> Q Clause
fromCon _ wrap _ _ m i (NormalC cn []) =
  clause
    [conP cn []]
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ appE (conE 'M1) $
      conE 'M1 `appE` (conE 'U1)) []
fromCon gk wrap _ (dt, vs) m i (NormalC cn fs) =
  -- runIO (putStrLn ("constructor " ++ show ix)) >>
  clause
    [conP cn (map (varP . field) [0..length fs - 1])]
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ conE 'M1 `appE`
      foldr1 prod (zipWith (fromField gk (dt, vs)) [0..] (map snd fs))) []
  where prod x y = conE '(:*:) `appE` x `appE` y
fromCon _ wrap _ _ m i (RecC cn []) =
  clause
    [conP cn []]
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ conE 'M1 `appE` (conE 'U1)) []
fromCon gk wrap _ (dt, vs) m i (RecC cn fs) =
  clause
    [conP cn (map (varP . field) [0..length fs - 1])]
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ conE 'M1 `appE`
      foldr1 prod (zipWith (fromField gk (dt, vs)) [0..] (map trd fs))) []
  where prod x y = conE '(:*:) `appE` x `appE` y
fromCon gk wrap ns (dt, vs) m i (InfixC t1 cn t2) =
  fromCon gk wrap ns (dt, vs) m i (NormalC cn [t1,t2])
fromCon _ _ _ _ _ _  (ForallC _ _ con) = forallCError con

errorClauseFrom :: Name -> Q Clause
errorClauseFrom dt =
  clause
    [wildP]
    (normalB $ appE (conE 'M1) $ varE 'error `appE` stringE
      ("No generic representation for empty datatype " ++ nameBase dt))
    []

fromField :: GenericKind -> (Name, [Name]) -> Int -> Type -> Q Exp
--fromField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conE 'I `appE` varE (field nr)
fromField gk _ nr t = conE 'M1 `appE` (fromFieldWrap gk nr =<< expandSyn t)

fromFieldWrap :: GenericKind -> Int -> Type -> Q Exp
fromFieldWrap _         _  ForallT{}  = rankNError
fromFieldWrap gk        nr (SigT t _) = fromFieldWrap gk nr t
fromFieldWrap Gen0      nr _          = conE 'K1 `appE` varE (field nr)
fromFieldWrap (Gen1 nb) nr t          = wC t nb  `appE` varE (field nr)

wC :: Type -> NameBase -> Q Exp
wC (VarT n) nb | NameBase n == nb = conE 'Par1
wC t nb
  | ground t nb = conE 'K1
  | otherwise =
      let _:tyArgs           = unapplyTy t
          numLastArgs        = min 1 $ length tyArgs
          (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

          inspectTy :: Type -> Q Exp
          inspectTy ForallT{} = rankNError
          inspectTy (SigT ty _) = inspectTy ty
          inspectTy (VarT a)
            | NameBase a == nb
            = conE 'Rec1
          inspectTy beta = infixApp (conE 'Comp1)
                                    (varE '(.))
                                    (varE 'fmap `appE` wC beta nb)

       in if any (not . (`ground` nb)) lhsArgs
             then outOfPlaceTyVarError
             else case rhsArgs of
                  []   -> conE 'K1
                  ty:_ -> inspectTy ty

toCon :: GenericKind -> (Q Pat -> Q Pat) -> Name -> (Name, [Name]) -> Int -> Int -> Con -> Q Clause
toCon _ wrap _ _ m i (NormalC cn []) =
    clause
      [wrap $ conP 'M1 [lrP m i $ conP 'M1 [conP 'M1 [conP 'U1 []]]]]
      (normalB $ conE cn) []
toCon gk wrap _ (dt, vs) m i (NormalC cn fs) =
    -- runIO (putStrLn ("constructor " ++ show ix)) >>
    clause
      [wrap $ conP 'M1 [lrP m i $ conP 'M1
        [foldr1 prod (zipWith (toField gk (dt, vs)) [0..] (map snd fs))]]]
      (normalB $ foldl appE (conE cn) (zipWith (toConUnwC gk) [0..] (map snd fs))) []
  where prod x y = conP '(:*:) [x,y]
toCon _ wrap _ _ m i (RecC cn []) =
    clause
      [wrap $ conP 'M1 [lrP m i $ conP 'M1 [conP 'U1 []]]]
      (normalB $ conE cn) []
toCon gk wrap _ (dt, vs) m i (RecC cn fs) =
    clause
      [wrap $ conP 'M1 [lrP m i $ conP 'M1
        [foldr1 prod (zipWith (toField gk (dt, vs)) [0..] (map trd fs))]]]
      (normalB $ foldl appE (conE cn) (zipWith (toConUnwC gk) [0..] (map trd fs))) []
  where prod x y = conP '(:*:) [x,y]
toCon gk wrap ns (dt, vs) m i (InfixC t1 cn t2) =
  toCon gk wrap ns (dt, vs) m i (NormalC cn [t1,t2])
toCon _ _ _ _ _ _ (ForallC _ _ con) = forallCError con

toConUnwC :: GenericKind -> Int -> Type -> Q Exp
toConUnwC Gen0      nr _ = varE $ field nr
toConUnwC (Gen1 nb) nr t = do
    t' <- expandSyn t
    unwC t' nb `appE` varE (field nr)

errorClauseTo :: Name -> Q Clause
errorClauseTo dt =
  clause
    [conP 'M1 [wildP]]
    (normalB $ varE 'error `appE` stringE
      ("No values for empty datatype " ++ nameBase dt))
    []

toField :: GenericKind -> (Name, [Name]) -> Int -> Type -> Q Pat
--toField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conP 'I [varP (field nr)]
toField gk _ nr _ = conP 'M1 [toFieldWrap gk nr]

toFieldWrap :: GenericKind -> Int -> Q Pat
toFieldWrap Gen0     nr = conP 'K1 [varP (field nr)]
toFieldWrap (Gen1 _) nr = varP (field nr)

field :: Int -> Name
field n = mkName $ "f" ++ show n

unwC :: Type -> NameBase -> Q Exp
unwC (SigT t _) nb = unwC t nb
unwC (VarT n) nb | NameBase n == nb = varE 'unPar1
unwC t nb
  | ground t nb = varE 'unK1
  | otherwise =
      let _:tyArgs           = unapplyTy t
          numLastArgs        = min 1 $ length tyArgs
          (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

          inspectTy :: Type -> Q Exp
          inspectTy ForallT{} = rankNError
          inspectTy (SigT ty _) = inspectTy ty
          inspectTy (VarT a)
            | NameBase a == nb
            = varE 'unRec1
          inspectTy beta = infixApp (varE 'fmap `appE` unwC beta nb)
                                    (varE '(.))
                                    (varE 'unComp1)

       in if any (not . (`ground` nb)) lhsArgs
             then outOfPlaceTyVarError
             else case rhsArgs of
                  []   -> varE 'unK1
                  ty:_ -> inspectTy ty

lrP :: Int -> Int -> (Q Pat -> Q Pat)
lrP 1 0 p = p
lrP _ 0 p = conP 'L1 [p]
lrP m i p = conP 'R1 [lrP (m-1) (i-1) p]

lrE :: Int -> Int -> (Q Exp -> Q Exp)
lrE 1 0 e = e
lrE _ 0 e = conE 'L1 `appE` e
lrE m i e = conE 'R1 `appE` lrE (m-1) (i-1) e

trd :: (a, b, c) -> c
trd (_,_,c) = c

-- | Variant of foldr1 which returns a special element for empty lists
foldr1' :: (a -> a -> a) -> a -> [a] -> a
foldr1' _ x [] = x
foldr1' _ _ [x] = x
foldr1' f x (h:t) = f h (foldr1' f x t)

-- | Credit to Víctor López Juan for this trick
sanitizeName :: String -> String
sanitizeName nb = 'N':(
    nb >>= \x -> case x of
      c | isAlphaNum c || c == '\''-> [c]
      '_' -> "__"
      c   -> "_" ++ show (ord c))

-- | Extracts the name of a constructor.
constructorName :: Con -> Name
constructorName (NormalC name      _  ) = name
constructorName (RecC    name      _  ) = name
constructorName (InfixC  _    name _  ) = name
constructorName (ForallC _    _    con) = constructorName con

outOfPlaceTyVarError :: a
outOfPlaceTyVarError = error $
    "Type applied to an argument involving the last parameter is not of kind * -> *"

-- | Deriving Generic(1) doesn't work with ExistentialQuantification
forallCError :: Con -> a
forallCError con = error $
  nameBase (constructorName con) ++ " must be a vanilla data constructor"

-- | A Generic1 instance must have at least one type variable of kind *
kindError :: a
kindError = error "Class Generic1 expects an argument of kind * -> *"

-- | Cannot have a constructor argument of form (forall a1 ... an. <type>)
-- when deriving Generic(1)
rankNError :: a
rankNError = error "Cannot have polymorphic arguments"

-- | Construct a type via curried application.
applyTy :: Type -> [Type] -> Type
applyTy = foldl' AppT

-- | Split an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would split to this:
--
-- @
-- [Either, Int, Char]
-- @
unapplyTy :: Type -> [Type]
unapplyTy = reverse . go
  where
    go :: Type -> [Type]
    go (AppT t1 t2) = t2:go t1
    go (SigT t _)   = go t
    go t            = [t]

#if MIN_VERSION_template_haskell(2,8,0)
-- | Split a type signature by the arrows on its spine. For example, this:
--
-- @
-- (Int -> String) -> Char -> ()
-- @
--
-- would split to this:
--
-- @
-- [Int -> String, Char, ()]
-- @
uncurryTy :: Type -> [Type]
uncurryTy (AppT (AppT ArrowT t1) t2) = t1:uncurryTy t2
uncurryTy (SigT t _)                 = uncurryTy t
uncurryTy t                          = [t]
#endif

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> [Kind]
#if MIN_VERSION_template_haskell(2,8,0)
uncurryKind = uncurryTy
#else
uncurryKind (ArrowK k1 k2) = k1:uncurryKind k2
uncurryKind k              = [k]
#endif

canRealizeKindStar :: Kind -> Bool
canRealizeKindStar k = case uncurryKind k of
    [k'] -> case k' of
#if MIN_VERSION_template_haskell(2,8,0)
                 StarT    -> True
                 (VarT _) -> True -- Kind k can be instantiated with *
#else
                 StarK    -> True
#endif
                 _ -> False
    _ -> False

-- | Indicates whether Generic (Gen0) or Generic1 (Gen1) is being derived.
data GenericKind = Gen0 | Gen1 NameBase

-------------------------------------------------------------------------------
-- NameBase
-------------------------------------------------------------------------------

-- | A wrapper around Name which only uses the nameBase (not the entire Name)
-- to compare for equality. For example, if you had two Names a_123 and a_456,
-- they are not equal as Names, but they are equal as NameBases.
--
-- This is useful when inspecting type variables, since a type variable in an
-- instance context may have a distinct Name from a type variable within an
-- actual constructor declaration, but we'd want to treat them as the same
-- if they have the same nameBase (since that's what the programmer uses to
-- begin with).
newtype NameBase = NameBase { getName :: Name }

getNameBase :: NameBase -> String
getNameBase = nameBase . getName

instance Eq NameBase where
    (==) = (==) `on` getNameBase

-- | True if the type does not mention the NameBase
ground :: Type -> NameBase -> Bool
ground (AppT t1 t2) nb = ground t1 nb && ground t2 nb
ground (SigT t _)   nb = ground t nb
ground (VarT n)     nb = NameBase n /= nb
ground ForallT{}    _  = rankNError
ground _            _  = True

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

-- | Expands all type synonyms in a type. Written by Dan Rosén in the
-- @genifunctors@ package (licensed under BSD3).
expandSyn :: Type -> Q Type
expandSyn (ForallT tvs ctx t) = fmap (ForallT tvs ctx) $ expandSyn t
expandSyn t@AppT{}            = expandSynApp t []
expandSyn t@ConT{}            = expandSynApp t []
expandSyn (SigT t _)          = expandSyn t   -- Ignore kind synonyms
expandSyn t                   = return t

expandSynApp :: Type -> [Type] -> Q Type
expandSynApp (AppT t1 t2) ts = do
    t2' <- expandSyn t2
    expandSynApp t1 (t2':ts)
expandSynApp (ConT n) ts | nameBase n == "[]" = return $ foldl' AppT ListT ts
expandSynApp t@(ConT n) ts = do
    info <- reify n
    case info of
        TyConI (TySynD _ tvs rhs) ->
            let (ts', ts'') = splitAt (length tvs) ts
                subs = mkSubst tvs ts'
                rhs' = subst subs rhs
             in expandSynApp rhs' ts''
        _ -> return $ foldl' AppT t ts
expandSynApp t ts = do
    t' <- expandSyn t
    return $ foldl' AppT t' ts

type Subst = Map Name Type

mkSubst :: [TyVarBndr] -> [Type] -> Subst
mkSubst vs ts =
   let vs' = map un vs
       un (PlainTV v)    = v
       un (KindedTV v _) = v
   in Map.fromList $ zip vs' ts

subst :: Subst -> Type -> Type
subst subs (ForallT v c t) = ForallT v c $ subst subs t
subst subs t@(VarT n)      = fromMaybe t $ Map.lookup n subs
subst subs (AppT t1 t2)    = AppT (subst subs t1) (subst subs t2)
subst subs (SigT t k)      = SigT (subst subs t) k
subst _ t                  = t
