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
-- library.
--
-- To use these functions, pass the name of a data type as an argument:
--
-- @
-- &#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
--
-- data Example a = Example Int Char a
-- $('deriveAll0'     ''Example) -- Derives Generic instance
-- $('deriveAll1'     ''Example) -- Derives Generic1 instance
-- $('deriveAll0And1' ''Example) -- Derives Generic and Generic1 instances
-- @
--
-- On GHC 7.4 or later, this code can also be used with data families. To derive
-- for a data family instance, pass the name of one of the instance's constructors:
--
-- @
-- &#123;-&#35; LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies &#35;-&#125;
--
-- data family Family a b
-- newtype instance Family Char b = FamilyChar Char
-- data    instance Family Bool b = FamilyTrue | FamilyFalse
--
-- $('deriveAll0' 'FamilyChar) -- instance Generic (Family Char b) where ...
-- $('deriveAll1' 'FamilyTrue) -- instance Generic1 (Family Bool) where ...
-- -- Alternatively, one could type $(deriveAll1 'FamilyFalse)
-- @
--
-- If you are deriving for data family instances, be aware of a bug on GHC
-- 7.8 (<https://ghc.haskell.org/trac/ghc/ticket/9692 Trac #9692>) which can
-- cause incorrectly derived 'Generic1' instances if a data family
-- declaration and one of its instances use different type variables:
--
-- @
-- data family Foo a b c
-- data instance Foo Int y z = Foo Int y z
-- $(deriveAll1 'Foo)
-- @
--
-- To avoid this issue, it is recommened that you use the same type variables
-- in the same positions in which they appeared in the data family declaration:
--
-- @
-- data family Foo a b c
-- data instance Foo Int b c = Foo Int b c
-- $(deriveAll1 'Foo)
-- @
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
     -- * @make@- functions
     -- $make
    , makeRep0
    , makeFrom
    , makeTo
    , makeRep1
    , makeFrom1
    , makeTo1
  ) where

import           Data.Char (isAlphaNum, ord)
import           Data.Function (on)
import           Data.List
import qualified Data.Map as Map
import           Data.Map as Map (Map)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Set (Set)

import           Generics.Deriving.Base

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax (Name(..), NameFlavour(..), Lift(..),
                                            modString, pkgString)
import           Language.Haskell.TH hiding (Fixity())

-- | Given the names of a generic class, a type to instantiate, a function in
-- the class and the default implementation, generates the code for a basic
-- generic instance.
simplInstance :: Name -> Name -> Name -> Name -> Q [Dec]
simplInstance cl ty fn df = do
  x <- newName "x"
  let typ = ForallT [PlainTV x] []
        ((foldl (\a -> AppT a . VarT . tyVarBndrToName) (ConT (genRepName 0 DataPlain ty)) []) `AppT` (VarT x))
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
deriveRep0 = deriveRepCommon 0

-- | Derive only the 'Rep1' type synonym. Not needed if 'deriveRepresentable1'
-- is used.
deriveRep1 :: Name -> Q [Dec]
deriveRep1 = deriveRepCommon 1

deriveRepCommon :: Int -> Name -> Q [Dec]
deriveRepCommon arity n = do
  i <- reifyDataInfo n
  let (name, _, allTvbs, cons, dv) = either error id i
      (tvbs, _, gk) = buildTypeInstance arity name allTvbs cons dv

  fmap (:[]) $ tySynD (genRepName arity dv name)
                      (map unKindedTV tvbs) -- The typechecker will infer the kinds of
                                            -- the TyVarBndrs in a type synonym
                                            -- declaration, so we don't need to
                                            -- splice them explicitly
                      (repType gk dv name cons)

deriveInst :: Name -> Q [Dec]
deriveInst = deriveInstCommon ''Generic ''Rep 0 'from 'to

deriveInst1 :: Name -> Q [Dec]
deriveInst1 = deriveInstCommon ''Generic1 ''Rep1 1 'from1 'to1

deriveInstCommon :: Name -> Name -> Int -> Name -> Name -> Name -> Q [Dec]
deriveInstCommon genericName repName arity fromName toName n = do
  i <- reifyDataInfo n
  let (name, _, allTvbs, cons, dv) = either error id i
      (tvbs, origTy, gk) = buildTypeInstance arity name allTvbs cons dv
      repTy = applyTyToTvbs (genRepName arity dv name) tvbs
#if __GLASGOW_HASKELL__ >= 707
      tyIns = TySynInstD repName (TySynEqn [origTy] repTy)
#else
      tyIns = TySynInstD repName [origTy] repTy
#endif
      mkBody maker = [clause [] (normalB $ mkCaseExp gk name cons maker) []]
      fcs = mkBody mkFrom
      tcs = mkBody mkTo

  fmap (:[]) $
    instanceD (cxt []) (conT genericName `appT` return origTy)
                         [return tyIns, funD fromName fcs, funD toName tcs]

{- $make

There are some data types for which the Template Haskell deriver functions in
this module are not sophisticated enough to infer the correct 'Generic' or
'Generic1' instances. As an example, consider this data type:

@
data Fix f a = Fix (f (Fix f a))
@

A proper 'Generic1' instance would look like this:

@
instance Functor f => Generic1 (Fix f) where ...
@

Unfortunately, 'deriveRepresentable1' cannot infer the @Functor f@ constraint.
One can still define a 'Generic1' instance for @Fix@, however, by using the
functions in this module that are prefixed with @make@-. For example:

@
$('deriveMeta' ''Fix)
$('deriveRep1' ''Fix)
instance Functor f => Generic1 (Fix f) where
  type Rep1 (Fix f) = $('makeRep1' ''Fix) f
  from1 = $('makeFrom1' ''Fix)
  to1   = $('makeTo1'   ''Fix)
@

Note that due to the lack of type-level lambdas in Haskell, one must manually
apply @$('makeRep1' ''Fix)@ to the type parameters of @Fix@ (@f@ in the above
example).

-}

-- | Generates the 'Rep0' type synonym constructor (as opposed to 'deriveRep0',
-- which generates the type synonym declaration).
makeRep0 :: Name -> Q Type
makeRep0 = makeRepCommon 0

-- | Generates the 'Rep1' type synonym constructor (as opposed to 'deriveRep1',
-- which generates the type synonym declaration).
makeRep1 :: Name -> Q Type
makeRep1 = makeRepCommon 1

makeRepCommon :: Int -> Name -> Q Type
makeRepCommon arity n = do
  i <- reifyDataInfo n
  case i of
    Left msg               -> error msg
    Right (name, _, _, _, dv) -> conT $ genRepName arity dv name

-- | Generates a lambda expression which behaves like 'from'.
makeFrom :: Name -> Q Exp
makeFrom = makeFunCommon mkFrom 0

-- | Generates a lambda expression which behaves like 'to'.
makeTo :: Name -> Q Exp
makeTo = makeFunCommon mkTo 0

-- | Generates a lambda expression which behaves like 'from1'.
makeFrom1 :: Name -> Q Exp
makeFrom1 = makeFunCommon mkFrom 1

-- | Generates a lambda expression which behaves like 'to1'.
makeTo1 :: Name -> Q Exp
makeTo1 = makeFunCommon mkTo 1

makeFunCommon :: (GenericKind -> Int -> Int -> Name -> [Con] -> [Q Match])
              -> Int -> Name -> Q Exp
makeFunCommon maker arity n = do
  i <- reifyDataInfo n
  let (name, _, allTvbs, cons, dv) = either error id i
      (_, _, gk) = buildTypeInstance arity name allTvbs cons dv
  mkCaseExp gk name cons maker

dataInstance :: Name -> Q [Dec]
dataInstance n = do
  i <- reifyDataInfo n
  case i of
    Left  _                    -> return []
    Right (n', isNT, _, _, dv) -> mkInstance n' dv isNT
  where
    mkInstance n' dv isNT = do
      ds <- mkDataData dv n'
      is <- mkDataInstance dv n' isNT
      return $ [ds,is]

constrInstance :: Name -> Q [Dec]
constrInstance n = do
  i <- reifyDataInfo n
  case i of
    Left  _               -> return []
    Right (n', _, _, cs, dv) -> mkInstance n' cs dv
  where
    mkInstance n' cs dv = do
      ds <- mapM (mkConstrData dv n') cs
      is <- mapM (mkConstrInstance dv n') cs
      return $ ds ++ is

selectInstance :: Name -> Q [Dec]
selectInstance n = do
  i <- reifyDataInfo n
  case i of
    Left  _               -> return []
    Right (n', _, _, cs, dv) -> mkInstance n' cs dv
  where
    mkInstance n' cs dv = do
      ds <- mapM (mkSelectData dv n') cs
      is <- mapM (mkSelectInstance dv n') cs
      return $ concat (ds ++ is)

tyVarBndrToName :: TyVarBndr -> Name
tyVarBndrToName (PlainTV  name)   = name
tyVarBndrToName (KindedTV name _) = name

tyVarBndrToNameBase :: TyVarBndr -> NameBase
tyVarBndrToNameBase = NameBase . tyVarBndrToName

tyVarBndrToKind :: TyVarBndr -> Kind
tyVarBndrToKind PlainTV{}      = starK
tyVarBndrToKind (KindedTV _ k) = k

stripRecordNames :: Con -> Con
stripRecordNames (RecC n f) =
  NormalC n (map (\(_, s, t) -> (s, t)) f)
stripRecordNames c = c

genName :: DataVariety -> [Name] -> Name
genName dv ns = mkName
              . showsDataVariety dv
              . intercalate "_"
              . consQualName
              $ map (sanitizeName . nameBase) ns
  where
    consQualName :: [String] -> [String]
    consQualName = case ns of
        []  -> id
        n:_ -> (showNameQual n :)

genRepName :: Int -> DataVariety -> Name -> Name
genRepName arity dv n = mkName
                      . showsDataVariety dv
                      . (("Rep" ++ show arity) ++)
                      . ((showNameQual n ++ "_") ++)
                      . sanitizeName
                      $ nameBase n

showsDataVariety :: DataVariety -> ShowS
showsDataVariety dv = (++ '_':label dv)
  where
    label DataPlain        = "Plain"
    label (DataFamily n _) = "Family_" ++ sanitizeName (nameBase n)

showNameQual :: Name -> String
showNameQual = sanitizeName . showQual
  where
    showQual (Name _ (NameQ m))       = modString m
    showQual (Name _ (NameG _ pkg m)) = pkgString pkg ++ ":" ++ modString m
    showQual _                        = ""

-- | Credit to Víctor López Juan for this trick
sanitizeName :: String -> String
sanitizeName nb = 'N':(
    nb >>= \x -> case x of
      c | isAlphaNum c || c == '\''-> [c]
      '_' -> "__"
      c   -> "_" ++ show (ord c))

mkDataData :: DataVariety -> Name -> Q Dec
mkDataData dv n = dataD (cxt []) (genName dv [n]) [] [] []

mkConstrData :: DataVariety -> Name -> Con -> Q Dec
mkConstrData dv dt (NormalC n _) =
  dataD (cxt []) (genName dv [dt, n]) [] [] []
mkConstrData dv dt r@(RecC _ _) =
  mkConstrData dv dt (stripRecordNames r)
mkConstrData dv dt (InfixC t1 n t2) =
  mkConstrData dv dt (NormalC n [t1,t2])
mkConstrData _ _ (ForallC _ _ con) = forallCError con

mkSelectData :: DataVariety -> Name -> Con -> Q [Dec]
mkSelectData dv dt (RecC n fs) = return (map one fs)
  where one (f, _, _) = DataD [] (genName dv [dt, n, f]) [] [] []
mkSelectData _ _ _ = return []


mkDataInstance :: DataVariety -> Name -> Bool -> Q Dec
mkDataInstance dv n _isNewtype =
  instanceD (cxt []) (appT (conT ''Datatype) (conT $ genName dv [n])) $
    [ funD 'datatypeName [clause [wildP] (normalB (stringE (nameBase n))) []]
    , funD 'moduleName   [clause [wildP] (normalB (stringE name)) []]
#if __GLASGOW_HASKELL__ >= 711
    , funD 'packageName  [clause [wildP] (normalB (stringE pkgName)) []]
#endif
    ]
#if __GLASGOW_HASKELL__ >= 708
 ++ if _isNewtype
       then [funD 'isNewtype [clause [wildP] (normalB (conE 'True)) []]]
       else []
#endif
  where
    name    = maybe (error "Cannot fetch module name!")  id (nameModule n)
#if __GLASGOW_HASKELL__ >= 711
    pkgName = maybe (error "Cannot fetch package name!") id (namePackage n)
    namePackage (Name _ (NameG _ pkg _)) = Just $ pkgString pkg
    namePackage _                        = Nothing
#endif

instance Lift Fixity where
  lift Prefix      = conE 'Prefix
  lift (Infix a n) = conE 'Infix `appE` [| a |] `appE` [| n |]

instance Lift Associativity where
  lift LeftAssociative  = conE 'LeftAssociative
  lift RightAssociative = conE 'RightAssociative
  lift NotAssociative   = conE 'NotAssociative

mkConstrInstance :: DataVariety -> Name -> Con -> Q Dec
mkConstrInstance dv dt (NormalC n _) = mkConstrInstanceWith dv dt n []
mkConstrInstance dv dt (RecC    n _) = mkConstrInstanceWith dv dt n
      [ funD 'conIsRecord [clause [wildP] (normalB (conE 'True)) []]]
mkConstrInstance dv dt (InfixC _ n _) =
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
      instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName dv [dt, n]))
        [funD 'conName   [clause [wildP] (normalB (stringE (nameBase n))) []],
         funD 'conFixity [clause [wildP] (normalB [| fi |]) []]]
  where
    convertFixity (Fixity n' d) = Infix (convertDirection d) n'
    convertDirection InfixL = LeftAssociative
    convertDirection InfixR = RightAssociative
    convertDirection InfixN = NotAssociative
mkConstrInstance _ _ (ForallC _ _ con) = forallCError con

mkConstrInstanceWith :: DataVariety -> Name -> Name -> [Q Dec] -> Q Dec
mkConstrInstanceWith dv dt n extra =
  instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName dv [dt, n]))
    (funD 'conName [clause [wildP] (normalB (stringE (nameBase n))) []] : extra)

mkSelectInstance :: DataVariety -> Name -> Con -> Q [Dec]
mkSelectInstance dv dt (RecC n fs) = return (map one fs) where
  one (f, _, _) =
    InstanceD ([]) (AppT (ConT ''Selector) (ConT $ genName dv [dt, n, f]))
      [FunD 'selName [Clause [WildP]
        (NormalB (LitE (StringL (nameBase f)))) []]]
mkSelectInstance _ _ _ = return []

repType :: GenericKind -> DataVariety -> Name -> [Con] -> Q Type
repType gk dv dt cs =
    conT ''D1 `appT` (conT $ genName dv [dt]) `appT`
      foldr1' sum' (conT ''V1)
        (map (repCon gk dv dt) cs)
  where
    sum' :: Q Type -> Q Type -> Q Type
    sum' a b = conT ''(:+:) `appT` a `appT` b

repCon :: GenericKind -> DataVariety -> Name -> Con -> Q Type
repCon _ dv dt (NormalC n []) =
    conT ''C1 `appT` (conT $ genName dv [dt, n]) `appT`
     (conT ''S1 `appT` conT ''NoSelector `appT` conT ''U1)
repCon gk dv dt (NormalC n fs) =
    conT ''C1 `appT` (conT $ genName dv [dt, n]) `appT`
     (foldr1 prod (map (repField gk . snd) fs)) where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b
repCon _ dv dt (RecC n []) =
    conT ''C1 `appT` (conT $ genName dv [dt, n]) `appT` conT ''U1
repCon gk dv dt (RecC n fs) =
    conT ''C1 `appT` (conT $ genName dv [dt, n]) `appT`
      (foldr1 prod (map (repField' gk dv dt n) fs)) where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b

repCon gk dv dt (InfixC t1 n t2) = repCon gk dv dt (NormalC n [t1,t2])
repCon _ _ _ (ForallC _ _ con) = forallCError con

--dataDeclToType :: (Name, [Name]) -> Type
--dataDeclToType (dt, vs) = foldl (\a b -> AppT a (VarT b)) (ConT dt) vs

repField :: GenericKind -> Type -> Q Type
--repField d t | t == dataDeclToType d = conT ''I
repField gk t = conT ''S1 `appT` conT ''NoSelector `appT`
                   (repFieldArg gk =<< expandSyn t)

repField' :: GenericKind -> DataVariety -> Name -> Name -> (Name, Strict, Type) -> Q Type
--repField' d ns (_, _, t) | t == dataDeclToType d = conT ''I
repField' gk dv dt ns (f, _, t) = conT ''S1 `appT` conT (genName dv [dt, ns, f]) `appT`
                                     (repFieldArg gk =<< expandSyn t)
-- Note: we should generate Par0 too, at some point

repFieldArg :: GenericKind -> Type -> Q Type
repFieldArg _ ForallT{} = rankNError
repFieldArg gk (SigT t _) = repFieldArg gk t
repFieldArg Gen0 t = conT ''Rec0 `appT` return t
repFieldArg (Gen1 nb) (VarT t) | NameBase t == nb = conT ''Par1
repFieldArg gk@(Gen1 nb) t = do
  let tyHead:tyArgs      = unapplyTy t
      numLastArgs        = min 1 $ length tyArgs
      (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs
      rec0Type           = conT ''Rec0  `appT` return t
      phiType            = return $ applyTyToTys tyHead lhsArgs

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

  itf <- isTyFamily tyHead
  if any (not . (`ground` nb)) lhsArgs
       || any (not . (`ground` nb)) tyArgs && itf
     then outOfPlaceTyVarError
     else case rhsArgs of
          []   -> rec0Type
          ty:_ -> inspectTy ty

mkCaseExp :: GenericKind -> Name -> [Con]
          -> (GenericKind -> Int -> Int -> Name -> [Con] -> [Q Match])
          -> Q Exp
mkCaseExp gk dt cs matchmaker = do
  val <- newName "val"
  lam1E (varP val) $ caseE (varE val) $ matchmaker gk 1 0 dt cs

mkFrom :: GenericKind -> Int -> Int -> Name -> [Con] -> [Q Match]
mkFrom _  _ _ dt [] = [errorFrom dt]
mkFrom gk m i _  cs = zipWith (fromCon gk wrapE (length cs)) [0..] cs
  where
    wrapE e = lrE m i e

errorFrom :: Name -> Q Match
errorFrom dt =
  match
    wildP
    (normalB $ appE (conE 'M1) $ varE 'error `appE` stringE
      ("No generic representation for empty datatype " ++ nameBase dt))
    []

errorTo :: Name -> Q Match
errorTo dt =
  match
    (conP 'M1 [wildP])
    (normalB $ varE 'error `appE` stringE
      ("No values for empty datatype " ++ nameBase dt))
    []

mkTo :: GenericKind -> Int -> Int -> Name -> [Con] -> [Q Match]
mkTo _  _ _ dt [] = [errorTo dt]
mkTo gk m i _  cs = zipWith (toCon gk wrapP (length cs)) [0..] cs
  where
    wrapP p = lrP m i p

fromCon :: GenericKind -> (Q Exp -> Q Exp) -> Int -> Int -> Con -> Q Match
fromCon _ wrap m i (NormalC cn []) =
  match
    (conP cn [])
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ appE (conE 'M1) $
      conE 'M1 `appE` (conE 'U1)) []
fromCon gk wrap m i (NormalC cn fs) =
  -- runIO (putStrLn ("constructor " ++ show ix)) >>
  match
    (conP cn (map (varP . field) [0..length fs - 1]))
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ conE 'M1 `appE`
      foldr1 prod (zipWith (fromField gk) [0..] (map snd fs))) []
  where prod x y = conE '(:*:) `appE` x `appE` y
fromCon _ wrap m i (RecC cn []) =
  match
    (conP cn [])
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ conE 'M1 `appE` (conE 'U1)) []
fromCon gk wrap m i (RecC cn fs) =
  match
    (conP cn (map (varP . field) [0..length fs - 1]))
    (normalB $ appE (conE 'M1) $ wrap $ lrE m i $ conE 'M1 `appE`
      foldr1 prod (zipWith (fromField gk) [0..] (map trd fs))) []
  where prod x y = conE '(:*:) `appE` x `appE` y
fromCon gk wrap m i (InfixC t1 cn t2) =
  fromCon gk wrap m i (NormalC cn [t1,t2])
fromCon _ _ _ _ (ForallC _ _ con) = forallCError con

fromField :: GenericKind -> Int -> Type -> Q Exp
--fromField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conE 'I `appE` varE (field nr)
fromField gk nr t = conE 'M1 `appE` (fromFieldWrap gk nr =<< expandSyn t)

fromFieldWrap :: GenericKind -> Int -> Type -> Q Exp
fromFieldWrap _         _  ForallT{}  = rankNError
fromFieldWrap gk        nr (SigT t _) = fromFieldWrap gk nr t
fromFieldWrap Gen0      nr _          = conE 'K1 `appE` varE (field nr)
fromFieldWrap (Gen1 nb) nr t          = wC t nb  `appE` varE (field nr)

wC :: Type -> NameBase -> Q Exp
wC (VarT n) nb | NameBase n == nb = conE 'Par1
wC t nb
  | ground t nb = conE 'K1
  | otherwise = do
      let tyHead:tyArgs       = unapplyTy t
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

      itf <- isTyFamily tyHead
      if any (not . (`ground` nb)) lhsArgs
           || any (not . (`ground` nb)) tyArgs && itf
         then outOfPlaceTyVarError
         else case rhsArgs of
              []   -> conE 'K1
              ty:_ -> inspectTy ty

toCon :: GenericKind -> (Q Pat -> Q Pat) -> Int -> Int -> Con -> Q Match
toCon _ wrap m i (NormalC cn []) =
    match
      (wrap $ conP 'M1 [lrP m i $ conP 'M1 [conP 'M1 [conP 'U1 []]]])
      (normalB $ conE cn) []
toCon gk wrap m i (NormalC cn fs) =
    -- runIO (putStrLn ("constructor " ++ show ix)) >>
    match
      (wrap $ conP 'M1 [lrP m i $ conP 'M1
        [foldr1 prod (zipWith (const . toField gk) [0..] fs)]])
      (normalB $ foldl appE (conE cn) (zipWith (toConUnwC gk) [0..] (map snd fs))) []
  where prod x y = conP '(:*:) [x,y]
toCon _ wrap m i (RecC cn []) =
    match
      (wrap $ conP 'M1 [lrP m i $ conP 'M1 [conP 'U1 []]])
      (normalB $ conE cn) []
toCon gk wrap m i (RecC cn fs) =
    match
      (wrap $ conP 'M1 [lrP m i $ conP 'M1
        [foldr1 prod (zipWith (const . toField gk) [0..] fs)]])
      (normalB $ foldl appE (conE cn) (zipWith (toConUnwC gk) [0..] (map trd fs))) []
  where prod x y = conP '(:*:) [x,y]
toCon gk wrap m i (InfixC t1 cn t2) =
  toCon gk wrap m i (NormalC cn [t1,t2])
toCon _ _ _ _ (ForallC _ _ con) = forallCError con

toConUnwC :: GenericKind -> Int -> Type -> Q Exp
toConUnwC Gen0      nr _ = varE $ field nr
toConUnwC (Gen1 nb) nr t = do
    t' <- expandSyn t
    unwC t' nb `appE` varE (field nr)

toField :: GenericKind -> Int -> Q Pat
--toField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conP 'I [varP (field nr)]
toField gk nr = conP 'M1 [toFieldWrap gk nr]

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
  | otherwise = do
      let tyHead:tyArgs      = unapplyTy t
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

      itf <- isTyFamily tyHead
      if any (not . (`ground` nb)) lhsArgs
           || any (not . (`ground` nb)) tyArgs && itf
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

-- | Extracts the name of a constructor.
constructorName :: Con -> Name
constructorName (NormalC name      _  ) = name
constructorName (RecC    name      _  ) = name
constructorName (InfixC  _    name _  ) = name
constructorName (ForallC _    _    con) = constructorName con

#if MIN_VERSION_template_haskell(2,7,0)
-- | Extracts the constructors of a data or newtype declaration.
dataDecCons :: Dec -> [Con]
dataDecCons (DataInstD    _ _ _ cons _) = cons
dataDecCons (NewtypeInstD _ _ _ con  _) = [con]
dataDecCons _ = error "Must be a data or newtype declaration."
#endif

-- | Boilerplate for top level splices.
--
-- The given Name must meet one of two criteria:
--
-- 1. It must be the name of a type constructor of a plain data type or newtype.
-- 2. It must be the name of a data family instance or newtype instance constructor.
--
-- Any other value will result in an exception.
reifyDataInfo :: Name
              -> Q (Either String (Name, Bool, [TyVarBndr], [Con], DataVariety))
reifyDataInfo name = do
  info <- reify name
  case info of
    TyConI dec ->
      return $ case dec of
        DataD    ctxt _ tvbs cons _ -> Right $
          checkDataContext name ctxt (name, False, tvbs, cons, DataPlain)
        NewtypeD ctxt _ tvbs con  _ -> Right $
          checkDataContext name ctxt (name, True, tvbs, [con], DataPlain)
        TySynD{} -> Left $ ns ++ "Type synonyms are not supported."
        _        -> Left $ ns ++ "Unsupported type: " ++ show dec
#if MIN_VERSION_template_haskell(2,7,0)
# if MIN_VERSION_template_haskell(2,11,0)
    DataConI _ _ parentName   -> do
# else
    DataConI _ _ parentName _ -> do
# endif
      parentInfo <- reify parentName
      return $ case parentInfo of
# if MIN_VERSION_template_haskell(2,11,0)
        FamilyI (DataFamilyD _ tvbs _) decs ->
# else
        FamilyI (FamilyD DataFam _ tvbs _) decs ->
# endif
          -- This isn't total, but the API requires that the data family instance have
          -- at least one constructor anyways, so this will always succeed.
          let instDec = flip find decs $ any ((name ==) . constructorName) . dataDecCons
           in case instDec of
                Just (DataInstD    ctxt _ instTys cons _) -> Right $
                  checkDataContext parentName ctxt
                    (parentName, False, tvbs, cons, DataFamily (constructorName $ head cons) instTys)
                Just (NewtypeInstD ctxt _ instTys con  _) -> Right $
                  checkDataContext parentName ctxt
                    (parentName, True, tvbs, [con], DataFamily (constructorName con) instTys)
                _ -> Left $ ns ++
                  "Could not find data or newtype instance constructor."
        _ -> Left $ ns ++ "Data constructor " ++ show name ++
          " is not from a data family instance constructor."
# if MIN_VERSION_template_haskell(2,11,0)
    FamilyI DataFamilyD{} _ ->
# else
    FamilyI (FamilyD DataFam _ _ _) _ ->
# endif
      return . Left $
        ns ++ "Cannot use a data family name. Use a data family instance constructor instead."
    _ -> return . Left $ ns ++ "The name must be of a plain data type constructor, "
                            ++ "or a data family instance constructor."
#else
    DataConI{} -> return . Left $ ns ++ "Cannot use a data constructor."
        ++ "\n\t(Note: if you are trying to derive for a data family instance, use GHC >= 7.4 instead.)"
    _          -> return . Left $ ns ++ "The name must be of a plain type constructor."
#endif
  where
    ns :: String
    ns = "Generics.Deriving.TH.reifyDataInfo: "

-- | Deduces the non-eta-reduced type variables, the instance type, the GenericKind
-- value to use for a Generic(1) instance.
buildTypeInstance :: Int
                  -- ^ Generic(0) or Generic1
                  -> Name
                  -- ^ The type constructor or data family name
                  -> [TyVarBndr]
                  -- ^ The type variables from the data type/data family declaration
                  -> [Con]
                  -- ^ The constructors of the data type/data family declaration
                  -> DataVariety
                  -- ^ If using a data family instance, provides the types used
                  -- to instantiate the instance
                  -> ([TyVarBndr], Type, GenericKind)
buildTypeInstance arity tyConName tvbs _ DataPlain
  | remainingLength < 0 || not (wellKinded droppedKinds) -- If we have enough well-kinded type variables
  = derivingKindError tyConName
  | otherwise = (remaining, instanceType, genericKindFromArity arity droppedNbs)
  where
    instanceType :: Type
    instanceType = applyTyToTvbs tyConName remaining

    remainingLength :: Int
    remainingLength = length tvbs - arity

    remaining, dropped :: [TyVarBndr]
    (remaining, dropped) = splitAt remainingLength tvbs

    droppedKinds :: [Kind]
    droppedKinds = map tyVarBndrToKind dropped

    droppedNbs :: [NameBase]
    droppedNbs = map tyVarBndrToNameBase dropped
buildTypeInstance arity parentName tvbs _cons (DataFamily _ instTysAndKinds)
  | remainingLength < 0 || not (wellKinded droppedKinds) -- If we have enough well-kinded type variables
  = derivingKindError parentName
  | canEtaReduce remaining dropped -- If it is safe to drop the type variables
  = (lhsTvbs, instanceType, genericKindFromArity arity droppedNbs)
  | otherwise = etaReductionError instanceType
  where
    -- We need to make sure that type variables in the instance head which have
    -- constraints aren't poly-kinded, e.g.,
    --
    -- @
    -- instance Generic (Foo (f :: k)) where
    -- @
    --
    -- To do this, we remove every kind ascription (i.e., strip off every 'SigT').
    instanceType :: Type
    instanceType = applyTyToTys (ConT parentName) $ map unSigT remaining

    remainingLength :: Int
    remainingLength = length tvbs - arity

    remaining, dropped :: [Type]
    (remaining, dropped) = splitAt remainingLength rhsTypes

    droppedKinds :: [Kind]
    droppedKinds = map tyVarBndrToKind . snd $ splitAt remainingLength tvbs

    droppedNbs :: [NameBase]
    droppedNbs = map varTToNameBase dropped

    -- We need to be mindful of an old GHC bug which causes kind variables to appear in
    -- @instTysAndKinds@ (as the name suggests) if
    --
    --   (1) @PolyKinds@ is enabled
    --   (2) either GHC 7.6 or 7.8 is being used (for more info, see Trac #9692).
    --
    -- Since Template Haskell doesn't seem to have a mechanism for detecting which
    -- language extensions are enabled, we do the next-best thing by counting
    -- the number of distinct kind variables in the data family declaration, and
    -- then dropping that number of entries from @instTysAndKinds@.
    instTypes :: [Type]
    instTypes =
#if __GLASGOW_HASKELL__ >= 710 || !(MIN_VERSION_template_haskell(2,8,0))
        instTysAndKinds
#else
        drop (Set.size . Set.unions $ map (distinctKindVars . tyVarBndrToKind) tvbs)
          instTysAndKinds
      where
        distinctKindVars :: Kind -> Set Name
# if MIN_VERSION_template_haskell(2,8,0)
        distinctKindVars (AppT k1 k2) = distinctKindVars k1 `Set.union` distinctKindVars k2
        distinctKindVars (SigT k _)   = distinctKindVars k
        distinctKindVars (VarT k)     = Set.singleton k
# endif
        distinctKindVars _            = Set.empty
#endif

    lhsTvbs :: [TyVarBndr]
    lhsTvbs = map (uncurry replaceTyVarName)
            . filter (isTyVar . snd)
            . take remainingLength
            $ zip tvbs rhsTypes

    -- In GHC 7.8, only the @Type@s up to the rightmost non-eta-reduced type variable
    -- in @instTypes@ are provided (as a result of a bug reported in Trac #9692). This
    -- is pretty inconvenient, as it makes it impossible to come up with the correct
    -- instance types in some cases. For example, consider the following code:
    --
    -- @
    -- data family Foo a b c
    -- data instance Foo Int y z = Foo Int y z
    -- $(deriveAll1 'Foo)
    -- @
    --
    -- Due to the aformentioned bug, Template Haskell doesn't tell us the names of
    -- either of type variables in the data instance (@y@ and @z@). As a result, we
    -- won't know to which fields of the 'Foo' constructor contain the rightmost type
    -- variable, which will result in an incorrect instance. Urgh.
    --
    -- A workaround is to ensure that you use the exact same type variables, in the
    -- exact same order, in the data family declaration and any data or newtype
    -- instances:
    --
    -- @
    -- data family Foo a b c
    -- data instance Foo Int b c = Foo Int b c
    -- $(deriveAll1 'Foo)
    -- @
    --
    -- Thankfully, other versions of GHC don't seem to have this bug.
    rhsTypes :: [Type]
    rhsTypes =
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
        instTypes ++ map tyVarBndrToType
                         (alignTyVarBndrs _cons $ drop (length instTypes) tvbs)
      where
        tyVarBndrToType :: TyVarBndr -> Type
        tyVarBndrToType (PlainTV n)    = VarT n
        tyVarBndrToType (KindedTV n k) = SigT (VarT n) k

        mapTyVarBndrName :: Name -> TyVarBndr -> TyVarBndr
        mapTyVarBndrName n PlainTV{}      = PlainTV n
        mapTyVarBndrName n (KindedTV _ k) = KindedTV n k

        -- To compensate for Trac #9692, we borrow some type variables from the data
        -- family declaration. However, the type variables used in a data family
        -- declaration are completely different from those used in a data family
        -- instance, even if their names appear to be the same. In particular,
        -- Template Haskell gives them different Name values.
        --
        -- We can account for this by walking through the constructors' type signatures
        -- to figure out what the correct Names should be, then replace the type
        -- variables from the data family declaration (which we borrow) with those
        -- from the constructors' type signatures.
        alignTyVarBndrs :: [Con] -> [TyVarBndr] -> [TyVarBndr]
        alignTyVarBndrs cons' tvbs' =
            let nbSet = Set.fromList $ map tyVarBndrToNameBase tvbs'
                nbMap = snd $ foldr alignCon (nbSet, Map.empty) cons'
            in map (\tvb -> mapTyVarBndrName (Map.findWithDefault
                                                (tyVarBndrToName     tvb)
                                                (tyVarBndrToNameBase tvb)
                                                nbMap
                                             ) tvb
                   ) tvbs'

        alignCon :: Con
                 -> (Set NameBase, Map NameBase Name)
                 -> (Set NameBase, Map NameBase Name)
        alignCon _ (nbs, m) | Set.null nbs = (nbs, m)
        alignCon (NormalC _ tys)    state = foldr alignTy state $ map snd tys
        alignCon (RecC    n tys)    state = alignCon (NormalC n $ map shrink tys) state
          where
            shrink (_, b, c) = (b, c)
        alignCon (InfixC ty1 n ty2) state = alignCon (NormalC n [ty1, ty2]) state
        alignCon (ForallC _ _ con)   _    = forallCError con

        alignTy :: Type
                -> (Set NameBase, Map NameBase Name)
                -> (Set NameBase, Map NameBase Name)
        alignTy _ (nbs, m) | Set.null nbs = (nbs, m)
        alignTy ForallT{}    _        = rankNError
        alignTy (AppT t1 t2) state    = alignTy t2 $ alignTy t1 state
        alignTy (SigT t _)   state    = alignTy t state
        alignTy (VarT n)     (nbs, m) =
            let nb = NameBase n
             in if nb `Set.member` nbs
                then let nbs' = nb `Set.delete` nbs
                         m'   = Map.insert nb n m
                      in (nbs', m')
                else (nbs, m)
        alignTy _            state    = state
#else
        instTypes
#endif

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> a
etaReductionError instanceType = error $
  "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
  ++ pprint instanceType

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: Name -> a
derivingKindError tyConName = error
  . showString "Cannot derive well-kinded instance of form ‘Generic1 "
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass Generic1 expects an argument of kind * -> *"
  $ ""

outOfPlaceTyVarError :: a
outOfPlaceTyVarError = error $
    "Type applied to an argument involving the last parameter is not of kind * -> *"

-- | Deriving Generic(1) doesn't work with ExistentialQuantification
forallCError :: Con -> a
forallCError con = error $
  nameBase (constructorName con) ++ " must be a vanilla data constructor"

-- | Cannot have a constructor argument of form (forall a1 ... an. <type>)
-- when deriving Generic(1)
rankNError :: a
rankNError = error "Cannot have polymorphic arguments"

-- | One cannot derive Generic(1) instance for anything that uses DatatypeContexts,
-- so check to make sure the Cxt field of a datatype is null.
checkDataContext :: Name -> Cxt -> a -> a
checkDataContext _        [] x = x
checkDataContext dataName _  _ = error $
  nameBase dataName ++ " must not have a datatype context"

-- | Is the given type a type family constructor (and not a data family constructor)?
isTyFamily :: Type -> Q Bool
isTyFamily (ConT n) = do
    info <- reify n
    return $ case info of
#if MIN_VERSION_template_haskell(2,11,0)
         FamilyI OpenTypeFamilyD{} _       -> True
#elif MIN_VERSION_template_haskell(2,7,0)
         FamilyI (FamilyD TypeFam _ _ _) _ -> True
#else
         TyConI  (FamilyD TypeFam _ _ _)   -> True
#endif
#if MIN_VERSION_template_haskell(2,9,0)
         FamilyI ClosedTypeFamilyD{} _     -> True
#endif
         _ -> False
isTyFamily _ = return False

-- | Construct a type via curried application.
applyTyToTys :: Type -> [Type] -> Type
applyTyToTys = foldl' AppT

-- | Apply a type constructor name to type variable binders.
applyTyToTvbs :: Name -> [TyVarBndr] -> Type
applyTyToTvbs = foldl' (\a -> AppT a . VarT . tyVarBndrToName) . ConT

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
                 VarT{}   -> True -- Kind k can be instantiated with *
#else
                 StarK    -> True
#endif
                 _ -> False
    _ -> False

wellKinded :: [Kind] -> Bool
wellKinded = all canRealizeKindStar

-- | Replace the Name of a TyVarBndr with one from a Type (if the Type has a Name).
replaceTyVarName :: TyVarBndr -> Type -> TyVarBndr
replaceTyVarName tvb            (SigT t _) = replaceTyVarName tvb t
replaceTyVarName PlainTV{}      (VarT n)   = PlainTV  n
replaceTyVarName (KindedTV _ k) (VarT n)   = KindedTV n k
replaceTyVarName tvb            _          = tvb

-- | Checks to see if the last types in a data family instance can be safely eta-
-- reduced (i.e., dropped), given the other types. This checks for three conditions:
--
-- (1) All of the dropped types are type variables
-- (2) All of the dropped types are distinct
-- (3) None of the remaining types mention any of the dropped types
canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
    && allDistinct nbs -- Make sure not to pass something of type [Type], since Type
                       -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsNameBase` nbs) remaining)
  where
    nbs :: [NameBase]
    nbs = map varTToNameBase dropped

-- | Extract the Name from a type variable.
varTToName :: Type -> Name
varTToName (VarT n)   = n
varTToName (SigT t _) = varTToName t
varTToName _          = error "Not a type variable!"

-- | Extract the NameBase from a type variable.
varTToNameBase :: Type -> NameBase
varTToNameBase = NameBase . varTToName

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar VarT{}     = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- | Peel off a kind signature from a TyVarBndr (if it has one).
unKindedTV :: TyVarBndr -> TyVarBndr
unKindedTV (KindedTV n _) = PlainTV n
unKindedTV tvb            = tvb

-- | Does the given type mention any of the NameBases in the list?
mentionsNameBase :: Type -> [NameBase] -> Bool
mentionsNameBase = go Set.empty
  where
    go :: Set NameBase -> Type -> [NameBase] -> Bool
    go foralls (ForallT tvbs _ t) nbs =
        go (foralls `Set.union` Set.fromList (map tyVarBndrToNameBase tvbs)) t nbs
    go foralls (AppT t1 t2) nbs = go foralls t1 nbs || go foralls t2 nbs
    go foralls (SigT t _)   nbs = go foralls t nbs
    go foralls (VarT n)     nbs = varNb `elem` nbs && not (varNb `Set.member` foralls)
      where
        varNb = NameBase n
    go _       _            _   = False

-- | Are all of the items in a list (which have an ordering) distinct?
--
-- This uses Set (as opposed to nub) for better asymptotic time complexity.
allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

-- | Indicates whether Generic (Gen0) or Generic1 (Gen1) is being derived. Gen1
-- bundles the Name of the last type parameter.
data GenericKind = Gen0 | Gen1 NameBase

-- | Construct a GenericKind value from its arity.
genericKindFromArity :: Int -> [NameBase] -> GenericKind
genericKindFromArity 0 _   = Gen0
genericKindFromArity 1 nbs = Gen1 $ head nbs
genericKindFromArity _ _   = error "Invalid arity"

-- | Indicates whether Generic(1) is being derived for a plain data type (DataPlain)
-- or a data family instance (DataFamily). DataFamily bundles the Name of the data
-- family instance's first constructor (for Name-generation purposes) and the types
-- used to instantiate the instance.
data DataVariety = DataPlain | DataFamily Name [Type]

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

instance Ord NameBase where
    compare = compare `on` getNameBase

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
   let vs' = map tyVarBndrToName vs
   in Map.fromList $ zip vs' ts

subst :: Subst -> Type -> Type
subst subs (ForallT v c t) = ForallT v c $ subst subs t
subst subs t@(VarT n)      = fromMaybe t $ Map.lookup n subs
subst subs (AppT t1 t2)    = AppT (subst subs t1) (subst subs t2)
subst subs (SigT t k)      = SigT (subst subs t) k
subst _ t                  = t
