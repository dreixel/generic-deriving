{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

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

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
import qualified Data.Map as Map
import           Data.Map as Map (Map)
#endif
#if __GLASGOW_HASKELL__ >= 706 && __GLASGOW_HASKELL__ < 710
import qualified Data.Set as Set
import           Data.Set (Set)
#endif

import           Generics.Deriving.TH.Internal
#if __GLASGOW_HASKELL__ >= 711
import           Generics.Deriving.TH.Post711
#else
import           Generics.Deriving.TH.Pre711
#endif

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH

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
      (sigE (varE undefinedValName) (return typ)))) []]]


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
  let (name, isNT, allTvbs, cons, dv) = either error id i
      (tvbs, _, gk) = buildTypeInstance arity name allTvbs cons dv

  fmap (:[]) $ tySynD (genRepName arity dv name)
                      (map unKindedTV tvbs) -- The typechecker will infer the kinds of
                                            -- the TyVarBndrs in a type synonym
                                            -- declaration, so we don't need to
                                            -- splice them explicitly
                      (repType gk dv name isNT cons)

deriveInst :: Name -> Q [Dec]
deriveInst = deriveInstCommon genericTypeName repTypeName 0 fromValName toValName

deriveInst1 :: Name -> Q [Dec]
deriveInst1 = deriveInstCommon generic1TypeName rep1TypeName 1 from1ValName to1ValName

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
    Left msg                  -> error msg
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
      !gk = trd3 $ buildTypeInstance arity name allTvbs cons dv
  mkCaseExp gk name cons maker

genRepName :: Int -> DataVariety -> Name -> Name
genRepName arity dv n = mkName
                      . showsDataVariety dv
                      . (("Rep" ++ show arity) ++)
                      . ((showNameQual n ++ "_") ++)
                      . sanitizeName
                      $ nameBase n

repType :: GenericKind -> DataVariety -> Name -> Bool -> [Con] -> Q Type
repType gk dv dt isNT cs =
    conT d1TypeName `appT` mkMetaDataType dv dt isNT `appT`
      foldr1' sum' (conT v1TypeName)
        (map (repCon gk dv dt) cs)
  where
    sum' :: Q Type -> Q Type -> Q Type
    sum' a b = conT sumTypeName `appT` a `appT` b

repCon :: GenericKind -> DataVariety -> Name -> Con -> Q Type
repCon _ dv dt (NormalC n []) =
    conT c1TypeName `appT` mkMetaConsType dv dt n False `appT` conT u1TypeName
repCon gk dv dt (NormalC n bts) = do
    let (bangs, ts) = unzip bts
    ssis <- reifySelStrictInfo n bangs
    conT c1TypeName `appT` mkMetaConsType dv dt n False `appT`
     (foldr1 prodT (zipWith (repField gk dv dt n Nothing) ssis ts))
repCon _ dv dt (RecC n []) =
    conT c1TypeName `appT` mkMetaConsType dv dt n True `appT` conT u1TypeName
repCon gk dv dt (RecC n vbts) = do
    let (selNames, bangs, ts) = unzip3 vbts
        selNames' = map Just selNames
    ssis <- reifySelStrictInfo n bangs
    conT c1TypeName `appT` mkMetaConsType dv dt n True `appT`
      (foldr1 prodT (zipWith3 (repField gk dv dt n) selNames' ssis ts))
repCon gk dv dt (InfixC t1 n t2) = repCon gk dv dt (NormalC n [t1,t2])
repCon _ _ _ con = gadtError con

prodT :: Q Type -> Q Type -> Q Type
prodT a b = conT productTypeName `appT` a `appT` b

repField :: GenericKind
         -> DataVariety
         -> Name
         -> Name
         -> Maybe Name
         -> SelStrictInfo
         -> Type
         -> Q Type
repField gk dv dt ns mbF ssi t =
           conT s1TypeName
    `appT` mkMetaSelType dv dt ns mbF ssi
    `appT` (repFieldArg gk =<< expandSyn t)

repFieldArg :: GenericKind -> Type -> Q Type
repFieldArg _ ForallT{} = rankNError
repFieldArg gk (SigT t _) = repFieldArg gk t
repFieldArg Gen0 t = boxT t
repFieldArg (Gen1 nb) (VarT t) | NameBase t == nb = conT par1TypeName
repFieldArg gk@(Gen1 nb) t = do
  let tyHead:tyArgs      = unapplyTy t
      numLastArgs        = min 1 $ length tyArgs
      (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs
      rec0Type           = boxT t
      phiType            = return $ applyTyToTys tyHead lhsArgs

      inspectTy :: Type -> Q Type
      inspectTy (VarT a)
        | NameBase a == nb
        = conT rec1TypeName `appT` phiType
      inspectTy (SigT ty _) = inspectTy ty
      inspectTy beta
        | not (ground beta nb)
        = conT composeTypeName `appT` phiType
                               `appT` repFieldArg gk beta
      inspectTy _ = rec0Type

  itf <- isTyFamily tyHead
  if any (not . (`ground` nb)) lhsArgs
       || any (not . (`ground` nb)) tyArgs && itf
     then outOfPlaceTyVarError
     else case rhsArgs of
          []   -> rec0Type
          ty:_ -> inspectTy ty

boxT :: Type -> Q Type
boxT ty = case unboxedRepNames ty of
    Just (boxTyName, _, _) -> conT boxTyName
    Nothing                -> conT rec0TypeName `appT` return ty

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
    (normalB $ appE (conE m1DataName) $ varE errorValName `appE` stringE
      ("No generic representation for empty datatype " ++ nameBase dt))
    []

errorTo :: Name -> Q Match
errorTo dt =
  match
    (conP m1DataName [wildP])
    (normalB $ varE errorValName `appE` stringE
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
    (normalB $ appE (conE m1DataName)
             $ wrap $ lrE m i $ conE m1DataName `appE` (conE u1DataName)) []
fromCon gk wrap m i (NormalC cn fs) =
  -- runIO (putStrLn ("constructor " ++ show ix)) >>
  match
    (conP cn (map (varP . field) [0..length fs - 1]))
    (normalB $ appE (conE m1DataName) $ wrap $ lrE m i $ conE m1DataName `appE`
      foldr1 prodE (zipWith (fromField gk) [0..] (map snd fs))) []
fromCon _ wrap m i (RecC cn []) =
  match
    (conP cn [])
    (normalB $ appE (conE m1DataName)
             $ wrap $ lrE m i $ conE m1DataName `appE` (conE u1DataName)) []
fromCon gk wrap m i (RecC cn fs) =
  match
    (conP cn (map (varP . field) [0..length fs - 1]))
    (normalB $ appE (conE m1DataName) $ wrap $ lrE m i $ conE m1DataName `appE`
      foldr1 prodE (zipWith (fromField gk) [0..] (map trd3 fs))) []
fromCon gk wrap m i (InfixC t1 cn t2) =
  fromCon gk wrap m i (NormalC cn [t1,t2])
fromCon _ _ _ _ con = gadtError con

prodE :: Q Exp -> Q Exp -> Q Exp
prodE x y = conE productDataName `appE` x `appE` y

fromField :: GenericKind -> Int -> Type -> Q Exp
fromField gk nr t = conE m1DataName `appE` (fromFieldWrap gk nr =<< expandSyn t)

fromFieldWrap :: GenericKind -> Int -> Type -> Q Exp
fromFieldWrap _         _  ForallT{}  = rankNError
fromFieldWrap gk        nr (SigT t _) = fromFieldWrap gk nr t
fromFieldWrap Gen0      nr t          = conE (boxRepName t) `appE` varE (field nr)
fromFieldWrap (Gen1 nb) nr t          = wC t nb             `appE` varE (field nr)

wC :: Type -> NameBase -> Q Exp
wC (VarT n) nb | NameBase n == nb = conE par1DataName
wC t nb
  | ground t nb = conE $ boxRepName t
  | otherwise = do
      let tyHead:tyArgs      = unapplyTy t
          numLastArgs        = min 1 $ length tyArgs
          (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

          inspectTy :: Type -> Q Exp
          inspectTy ForallT{} = rankNError
          inspectTy (SigT ty _) = inspectTy ty
          inspectTy (VarT a)
            | NameBase a == nb
            = conE rec1DataName
          inspectTy beta = infixApp (conE comp1DataName)
                                    (varE composeValName)
                                    (varE fmapValName `appE` wC beta nb)

      itf <- isTyFamily tyHead
      if any (not . (`ground` nb)) lhsArgs
           || any (not . (`ground` nb)) tyArgs && itf
         then outOfPlaceTyVarError
         else case rhsArgs of
              []   -> conE $ boxRepName t
              ty:_ -> inspectTy ty

boxRepName :: Type -> Name
boxRepName = maybe k1DataName snd3 . unboxedRepNames

toCon :: GenericKind -> (Q Pat -> Q Pat) -> Int -> Int -> Con -> Q Match
toCon _ wrap m i (NormalC cn []) =
    match
      (wrap $ conP m1DataName [lrP m i $ conP m1DataName [conP u1DataName []]])
      (normalB $ conE cn) []
toCon gk wrap m i (NormalC cn fs) =
    match
      (wrap $ conP m1DataName [lrP m i $ conP m1DataName
        [foldr1 prod (zipWith (\nr -> toField gk nr . snd) [0..] fs)]])
      (normalB $ foldl appE (conE cn) (zipWith (\nr t -> expandSyn t >>= toConUnwC gk nr)
                                      [0..] (map snd fs))) []
  where prod x y = conP productDataName [x,y]
toCon _ wrap m i (RecC cn []) =
    match
      (wrap $ conP m1DataName [lrP m i $ conP m1DataName [conP u1DataName []]])
      (normalB $ conE cn) []
toCon gk wrap m i (RecC cn fs) =
    match
      (wrap $ conP m1DataName [lrP m i $ conP m1DataName
        [foldr1 prod (zipWith (\nr (_, _, t) -> toField gk nr t) [0..] fs)]])
      (normalB $ foldl appE (conE cn) (zipWith (\nr t -> expandSyn t >>= toConUnwC gk nr)
                                               [0..] (map trd3 fs))) []
  where prod x y = conP productDataName [x,y]
toCon gk wrap m i (InfixC t1 cn t2) =
  toCon gk wrap m i (NormalC cn [t1,t2])
toCon _ _ _ _ con = gadtError con

toConUnwC :: GenericKind -> Int -> Type -> Q Exp
toConUnwC Gen0      nr _ = varE $ field nr
toConUnwC (Gen1 nb) nr t = unwC t nb `appE` varE (field nr)

toField :: GenericKind -> Int -> Type -> Q Pat
toField gk nr t = conP m1DataName [toFieldWrap gk nr t]

toFieldWrap :: GenericKind -> Int -> Type -> Q Pat
toFieldWrap Gen0     nr t = conP (boxRepName t) [varP (field nr)]
toFieldWrap (Gen1 _) nr _ = varP (field nr)

field :: Int -> Name
field n = mkName $ "f" ++ show n

unwC :: Type -> NameBase -> Q Exp
unwC (SigT t _) nb = unwC t nb
unwC (VarT n) nb | NameBase n == nb = varE unPar1ValName
unwC t nb
  | ground t nb = varE $ unboxRepName t
  | otherwise = do
      let tyHead:tyArgs      = unapplyTy t
          numLastArgs        = min 1 $ length tyArgs
          (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

          inspectTy :: Type -> Q Exp
          inspectTy ForallT{} = rankNError
          inspectTy (SigT ty _) = inspectTy ty
          inspectTy (VarT a)
            | NameBase a == nb
            = varE unRec1ValName
          inspectTy beta = infixApp (varE fmapValName `appE` unwC beta nb)
                                    (varE composeValName)
                                    (varE unComp1ValName)

      itf <- isTyFamily tyHead
      if any (not . (`ground` nb)) lhsArgs
           || any (not . (`ground` nb)) tyArgs && itf
         then outOfPlaceTyVarError
         else case rhsArgs of
              []   -> varE $ unboxRepName t
              ty:_ -> inspectTy ty

unboxRepName :: Type -> Name
unboxRepName = maybe unK1ValName trd3 . unboxedRepNames

lrP :: Int -> Int -> (Q Pat -> Q Pat)
lrP 1 0 p = p
lrP _ 0 p = conP l1DataName [p]
lrP m i p = conP r1DataName [lrP (m-1) (i-1) p]

lrE :: Int -> Int -> (Q Exp -> Q Exp)
lrE 1 0 e = e
lrE _ 0 e = conE l1DataName `appE` e
lrE m i e = conE r1DataName `appE` lrE (m-1) (i-1) e

unboxedRepNames :: Type -> Maybe (Name, Name, Name)
unboxedRepNames ty
  | ty == ConT addrHashTypeName   = Just (uAddrTypeName,   uAddrDataName,   uAddrHashValName)
  | ty == ConT charHashTypeName   = Just (uCharTypeName,   uCharDataName,   uCharHashValName)
  | ty == ConT doubleHashTypeName = Just (uDoubleTypeName, uDoubleDataName, uDoubleHashValName)
  | ty == ConT floatHashTypeName  = Just (uFloatTypeName,  uFloatDataName,  uFloatHashValName)
  | ty == ConT intHashTypeName    = Just (uIntTypeName,    uIntDataName,    uIntHashValName)
  | ty == ConT wordHashTypeName   = Just (uWordTypeName,   uWordDataName,   uWordHashValName)
  | otherwise                     = Nothing

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
        alignCon (InfixC ty1 n ty2) state = alignCon (NormalC n [ty1, ty2]) state
        alignCon con _ = gadtError con

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

-- | True if the type does not mention the NameBase
ground :: Type -> NameBase -> Bool
ground (AppT t1 t2) nb = ground t1 nb && ground t2 nb
ground (SigT t _)   nb = ground t nb
ground (VarT n)     nb = NameBase n /= nb
ground ForallT{}    _  = rankNError
ground _            _  = True

-- | Indicates whether Generic (Gen0) or Generic1 (Gen1) is being derived. Gen1
-- bundles the Name of the last type parameter.
data GenericKind = Gen0 | Gen1 NameBase

-- | Construct a GenericKind value from its arity.
genericKindFromArity :: Int -> [NameBase] -> GenericKind
genericKindFromArity 0 _   = Gen0
genericKindFromArity 1 nbs = Gen1 $ head nbs
genericKindFromArity _ _   = error "Invalid arity"
