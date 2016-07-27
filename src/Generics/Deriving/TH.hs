{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

{- |
Module      :  Generics.Deriving.TH
Copyright   :  (c) 2008--2009 Universiteit Utrecht
License     :  BSD3

Maintainer  :  generics@haskell.org
Stability   :  experimental
Portability :  non-portable

This module contains Template Haskell code that can be used to
automatically generate the boilerplate code for the generic deriving
library.

To use these functions, pass the name of a data type as an argument:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;

data Example a = Example Int Char a
$('deriveAll0'     ''Example) -- Derives Generic instance
$('deriveAll1'     ''Example) -- Derives Generic1 instance
$('deriveAll0And1' ''Example) -- Derives Generic and Generic1 instances
@

On GHC 7.4 or later, this code can also be used with data families. To derive
for a data family instance, pass the name of one of the instance's constructors:

@
&#123;-&#35; LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies &#35;-&#125;

data family Family a b
newtype instance Family Char x = FamilyChar Char
data    instance Family Bool x = FamilyTrue | FamilyFalse

$('deriveAll0' 'FamilyChar) -- instance Generic (Family Char b) where ...
$('deriveAll1' 'FamilyTrue) -- instance Generic1 (Family Bool) where ...
-- Alternatively, one could type $(deriveAll1 'FamilyFalse)
@
-}

-- Adapted from Generics.Regular.TH
module Generics.Deriving.TH (

      deriveMeta
    , deriveData
    , deriveConstructors
    , deriveSelectors

    , deriveAll
    , deriveAll0
    , deriveAll1
    , deriveAll0And1
    , deriveRepresentable0
    , deriveRepresentable1
    , deriveRep0
    , deriveRep1
    , simplInstance
     -- * -@WithSigs@ functions
     -- $withSigs
    , deriveAll0WithKindSigs
    , deriveAll1WithKindSigs
    , deriveAll0And1WithKindSigs
    , deriveRepresentable0WithKindSigs
    , deriveRepresentable1WithKindSigs
    , deriveRep0WithKindSigs
    , deriveRep1WithKindSigs
     -- * @make@- functions
     -- $make
    , makeRep0
    , makeRep0FromType
    , makeFrom
    , makeFrom0
    , makeTo
    , makeTo0
    , makeRep1
    , makeRep1FromType
    , makeFrom1
    , makeTo1
  ) where

import           Control.Monad ((>=>), unless, when)

#if MIN_VERSION_template_haskell(2,8,0) && !(MIN_VERSION_template_haskell(2,10,0))
import           Data.Foldable (foldr')
#endif
import           Data.List (nub)
import qualified Data.Map as Map (fromList)

import           Generics.Deriving.TH.Internal
#if MIN_VERSION_base(4,9,0)
import           Generics.Deriving.TH.Post4_9
#else
import           Generics.Deriving.TH.Pre4_9
#endif

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH

{- $withSigs
By default, 'deriveRep0', 'deriveRep1', and functions that invoke it generate
type synonyms whose type variable binders do not have explicit kind binders for
polykinded type variables. This is a pretty reasonable default, since puts less
of a burden on the Template Haskell machinery to get the kinds just right, and
lets the kind inferencer do more work. However, there are times when you want to
have explicit kind signatures, such as if you have a datatype that uses
@-XTypeInType@. For example:

@
data Prox (a :: k) (b :: *) = Prox k
$('deriveRep0WithKindSigs' ''Prox)
@

will result in something like:

@
type Rep0Prox (a :: k) (b :: *) = Rec0 k
@

Whereas if you had used 'deriveRep0', you would have something like:

@
type Rep0Prox a (b :: *) = Rec0 k
@

which will fail to compile, since k is out-of-scope!
-}

-- | Given the names of a generic class, a type to instantiate, a function in
-- the class and the default implementation, generates the code for a basic
-- generic instance.
simplInstance :: Name -> Name -> Name -> Name -> Q [Dec]
simplInstance cl ty fn df = do
  x <- newName "x"
  let typ = ForallT [PlainTV x] []
        ((foldl (\a -> AppT a . VarT . tyVarBndrName) (ConT (genRepName Generic DataPlain ty)) []) `AppT` (VarT x))
  fmap (: []) $ instanceD (cxt []) (conT cl `appT` conT ty)
    [funD fn [clause [] (normalB (varE df `appE`
      (sigE (varE undefinedValName) (return typ)))) []]]

-- | A backwards-compatible synonym for 'deriveAll0'.
deriveAll :: Name -> Q [Dec]
deriveAll = deriveAll0

-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, the 'Selector'
-- instances, and the 'Representable0' instance.
deriveAll0 :: Name -> Q [Dec]
deriveAll0 = deriveAllCommon True False False

-- | Like 'deriveAll0', except that the type variable binders in the
-- 'Rep' type synonym will have explicit kind signatures.
deriveAll0WithKindSigs :: Name -> Q [Dec]
deriveAll0WithKindSigs = deriveAllCommon True False True

-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, the 'Selector'
-- instances, and the 'Representable1' instance.
deriveAll1 :: Name -> Q [Dec]
deriveAll1 = deriveAllCommon False True False

-- | Like 'deriveAll1', except that the type variable binders in the
-- 'Rep1' type synonym will have explicit kind signatures.
deriveAll1WithKindSigs :: Name -> Q [Dec]
deriveAll1WithKindSigs = deriveAllCommon False True True

-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, the 'Selector'
-- instances, the 'Representable0' instance, and the 'Representable1' instance.
deriveAll0And1 :: Name -> Q [Dec]
deriveAll0And1 = deriveAllCommon True True False

-- | Like 'deriveAll0And1', except that the type variable binders in the
-- 'Rep' and 'Rep1' type synonyms will have explicit kind signatures.
deriveAll0And1WithKindSigs :: Name -> Q [Dec]
deriveAll0And1WithKindSigs = deriveAllCommon True True True

deriveAllCommon :: Bool -> Bool -> Bool -> Name -> Q [Dec]
deriveAllCommon generic generic1 useKindSigs n = do
    a <- deriveMeta n
    b <- if generic
            then deriveRepresentableCommon Generic useKindSigs n
            else return []
    c <- if generic1
            then deriveRepresentableCommon Generic1 useKindSigs n
            else return []
    return (a ++ b ++ c)

-- | Given the type and the name (as string) for the Representable0 type
-- synonym to derive, generate the 'Representable0' instance.
deriveRepresentable0 :: Name -> Q [Dec]
deriveRepresentable0 = deriveRepresentableCommon Generic False

-- | Like 'deriveRepresentable0', except that the type variable binders in the
-- 'Rep' type synonym will have explicit kind signatures.
deriveRepresentable0WithKindSigs :: Name -> Q [Dec]
deriveRepresentable0WithKindSigs = deriveRepresentableCommon Generic True

-- | Given the type and the name (as string) for the Representable1 type
-- synonym to derive, generate the 'Representable1' instance.
deriveRepresentable1 :: Name -> Q [Dec]
deriveRepresentable1 = deriveRepresentableCommon Generic1 False

-- | Like 'deriveRepresentable1', except that the type variable binders in the
-- 'Rep1' type synonym will have explicit kind signatures.
deriveRepresentable1WithKindSigs :: Name -> Q [Dec]
deriveRepresentable1WithKindSigs = deriveRepresentableCommon Generic1 True

deriveRepresentableCommon :: GenericClass -> Bool -> Name -> Q [Dec]
deriveRepresentableCommon gClass useKindSigs n = do
    rep  <- deriveRepCommon gClass useKindSigs n
    inst <- deriveInst gClass n
    return (rep ++ inst)

-- | Derive only the 'Rep0' type synonym. Not needed if 'deriveRepresentable0'
-- is used.
deriveRep0 :: Name -> Q [Dec]
deriveRep0 = deriveRepCommon Generic False

-- | Like 'deriveRep0', except that the type variable binders in the 'Rep'
-- type synonym will have explicit kind signatures.
deriveRep0WithKindSigs :: Name -> Q [Dec]
deriveRep0WithKindSigs = deriveRepCommon Generic True

-- | Derive only the 'Rep1' type synonym. Not needed if 'deriveRepresentable1'
-- is used.
deriveRep1 :: Name -> Q [Dec]
deriveRep1 = deriveRepCommon Generic1 False

-- | Like 'deriveRep1', except that the type variable binders in the 'Rep1'
-- type synonym will have explicit kind signatures.
deriveRep1WithKindSigs :: Name -> Q [Dec]
deriveRep1WithKindSigs = deriveRepCommon Generic1 True

deriveRepCommon :: GenericClass -> Bool -> Name -> Q [Dec]
deriveRepCommon gClass useKindSigs n = do
  i <- reifyDataInfo n
  let (name, isNT, declTvbs, cons, dv) = either error id i
  -- See Note [Forcing buildTypeInstance]
  !_ <- buildTypeInstance gClass name declTvbs dv

  tySynTvbs <- grabTyVarBndrsFromCons gClass cons
  let tySynTvbs' = if useKindSigs
                      then tySynTvbs
                      else map (\tvb -> if isKindMonomorphic (tyVarBndrKind tvb)
                                           then tvb
                                           else unKindedTV tvb) tySynTvbs
  fmap (:[]) $ tySynD (genRepName gClass dv name)
                      tySynTvbs'
                      -- The typechecker will infer the kinds of the TyVarBndrs
                      -- in a type synonym declaration, so we don't need to
                      -- splice them in explicitly (hence the unKindedTV call).
                      (repType gClass dv name isNT cons tySynTvbs)

deriveInst :: GenericClass -> Name -> Q [Dec]
deriveInst Generic  = deriveInstCommon genericTypeName  repTypeName  Generic  fromValName  toValName
deriveInst Generic1 = deriveInstCommon generic1TypeName rep1TypeName Generic1 from1ValName to1ValName

deriveInstCommon :: Name -> Name -> GenericClass -> Name -> Name -> Name -> Q [Dec]
deriveInstCommon genericName repName gClass fromName toName n = do
  i <- reifyDataInfo n
  let (name, _, allTvbs, cons, dv) = either error id i
  origTy      <- buildTypeInstance gClass name allTvbs dv
  repTySynApp <- makeRepTySynApp gClass dv name cons origTy
  let tyIns = TySynInstD repName
#if MIN_VERSION_template_haskell(2,9,0)
                         (TySynEqn [origTy] repTySynApp)
#else
                         [origTy] repTySynApp
#endif
      mkBody maker = [clause [] (normalB $ mkCaseExp gClass name cons maker) []]
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
  type Rep1 (Fix f) = $('makeRep1FromType' ''Fix [t| Fix f |])
  from1 = $('makeFrom1' ''Fix)
  to1   = $('makeTo1'   ''Fix)
@

Note that due to the lack of type-level lambdas in Haskell, one must manually
apply @'makeRep1FromType' ''Fix@ to the type @Fix f@.

Be aware that there is a bug on GHC 7.0, 7.2, and 7.4 which might prevent you from
using 'makeRep0FromType' and 'makeRep1FromType'. In the @Fix@ example above, you
would experience the following error:

@
    Kinded thing `f' used as a type
    In the Template Haskell quotation [t| Fix f |]
@

Then a workaround is to use 'makeRep1' instead, which requires you to pass as
arguments the type variables that occur in the instance, in order from left to
right, excluding duplicates. (Normally, 'makeRep1FromType' would figure this
out for you.) Using the above example:

@
$('deriveMeta' ''Fix)
$('deriveRep1' ''Fix)
instance Functor f => Generic1 (Fix f) where
  type Rep1 (Fix f) = $('makeRep1' ''Fix) f
  from1 = $('makeFrom1' ''Fix)
  to1   = $('makeTo1'   ''Fix)
@

On GHC 7.4, you might encounter more complicated examples involving data
families. For instance:

@
data family Fix a b c d
newtype instance Fix b (f c) (g b) a = Fix (f (Fix b (f c) (g b) a))

$('deriveMeta' ''Fix)
$('deriveRep1' ''Fix)
instance Functor f => Generic1 (Fix b (f c) (g b)) where
  type Rep1 (Fix b (f c) (g b)) = $('makeRep1' 'Fix) b f c g
  from1 = $('makeFrom1' 'Fix)
  to1   = $('makeTo1'   'Fix)
@

Note that you don't pass @b@ twice, only once.
-}

-- | Generates the 'Rep' type synonym constructor (as opposed to 'deriveRep0',
-- which generates the type synonym declaration). After splicing it into
-- Haskell source, it expects types as arguments. For example:
--
-- @
-- type Rep (Foo a b) = $('makeRep0' ''Foo) a b
-- @
makeRep0 :: Name -> Q Type
makeRep0 n = makeRepCommon Generic n Nothing

-- | Generates the 'Rep1' type synonym constructor (as opposed to 'deriveRep1',
-- which generates the type synonym declaration). After splicing it into
-- Haskell source, it expects types as arguments. For example:
--
-- @
-- type Rep1 (Foo a b) = $('makeRep1' ''Foo) a b
-- @
makeRep1 :: Name -> Q Type
makeRep1 n = makeRepCommon Generic1 n Nothing

-- | Generates the 'Rep' type synonym constructor (as opposed to 'deriveRep0',
-- which generates the type synonym declaration) applied to its type arguments.
-- Unlike 'makeRep0', this also takes a quoted 'Type' as an argument, e.g.,
--
-- @
-- type Rep (Foo a b) = $('makeRep0FromType' ''Foo [t| Foo a b |])
-- @
makeRep0FromType :: Name -> Q Type -> Q Type
makeRep0FromType n = makeRepCommon Generic n . Just

-- | Generates the 'Rep1' type synonym constructor (as opposed to 'deriveRep1',
-- which generates the type synonym declaration) applied to its type arguments.
-- Unlike 'makeRep1', this also takes a quoted 'Type' as an argument, e.g.,
--
-- @
-- type Rep1 (Foo a b) = $('makeRep1FromType' ''Foo [t| Foo a b |])
-- @
makeRep1FromType :: Name -> Q Type -> Q Type
makeRep1FromType n = makeRepCommon Generic1 n . Just

makeRepCommon :: GenericClass
              -> Name
              -> Maybe (Q Type)
              -> Q Type
makeRepCommon gClass n mbQTy = do
  i <- reifyDataInfo n
  let (name, _, _, cons, dv) = either error id i
  case mbQTy of
       Just qTy -> do
           ty <- qTy
           makeRepTySynApp gClass dv name cons ty
       Nothing -> conT $ genRepName gClass dv name

makeRepTySynApp :: GenericClass
                -> DataVariety
                -> Name
                -> [Con]
                -> Type
                -> Q Type
makeRepTySynApp gClass dv name cons ty = do
  -- Here, we figure out the distinct type variables (in order from left-to-right)
  -- of the LHS of the Rep(1) instance. We call unKindedTV because the kind
  -- inferencer can figure out the kinds perfectly well, so we don't need to
  -- give anything here explicit kind signatures.
  let instTvbs = nub . map unKindedTV $ visibleTyVarsOfType ty
  -- We grab the type variables from the first constructor's type signature.
  -- Or, if there are no constructors, we grab no type variables. The latter
  -- is okay because we use zipWith to ensure that we never pass more type
  -- variables than the generated type synonym can accept.
  -- See Note [Arguments to generated type synonyms]
  tySynTvbs <- grabTyVarBndrsFromCons gClass cons
  return . applyTyToTvbs (genRepName gClass dv name)
         $ zipWith const instTvbs tySynTvbs

-- | A backwards-compatible synonym for 'makeFrom0'.
makeFrom :: Name -> Q Exp
makeFrom = makeFrom0

-- | Generates a lambda expression which behaves like 'from'.
makeFrom0 :: Name -> Q Exp
makeFrom0 = makeFunCommon mkFrom Generic

-- | A backwards-compatible synonym for 'makeTo0'.
makeTo :: Name -> Q Exp
makeTo = makeTo0

-- | Generates a lambda expression which behaves like 'to'.
makeTo0 :: Name -> Q Exp
makeTo0 = makeFunCommon mkTo Generic

-- | Generates a lambda expression which behaves like 'from1'.
makeFrom1 :: Name -> Q Exp
makeFrom1 = makeFunCommon mkFrom Generic1

-- | Generates a lambda expression which behaves like 'to1'.
makeTo1 :: Name -> Q Exp
makeTo1 = makeFunCommon mkTo Generic1

makeFunCommon :: (GenericClass -> Int -> Int -> Name -> [Con] -> Q Match)
              -> GenericClass -> Name -> Q Exp
makeFunCommon maker gClass n = do
  i <- reifyDataInfo n
  let (name, _, allTvbs, cons, dv) = either error id i
  -- See Note [Forcing buildTypeInstance]
  buildTypeInstance gClass name allTvbs dv
    `seq` mkCaseExp gClass name cons maker

genRepName :: GenericClass -> DataVariety -> Name -> Name
genRepName gClass dv n = mkName
                      . showsDataVariety dv
                      . (("Rep" ++ show (fromEnum gClass)) ++)
                      . ((showNameQual n ++ "_") ++)
                      . sanitizeName
                      $ nameBase n

repType :: GenericClass
        -> DataVariety
        -> Name
        -> Bool
        -> [Con]
        -> [TyVarBndr]
        -> Q Type
repType gClass dv dt isNT cs tySynTvbs =
    conT d1TypeName `appT` mkMetaDataType dv dt isNT `appT`
      foldr1' sum' (conT v1TypeName)
        (map (repCon gClass dv dt tySynTvbs) cs)
  where
    sum' :: Q Type -> Q Type -> Q Type
    sum' a b = conT sumTypeName `appT` a `appT` b

repCon :: GenericClass
       -> DataVariety
       -> Name
       -> [TyVarBndr]
       -> Con
       -> Q Type
repCon gClass dv dt tySynTvbs (NormalC n bts) = do
    let bangs = map fst bts
    ssis <- reifySelStrictInfo n bangs
    repConWith gClass dv dt n tySynTvbs Nothing ssis False False
repCon gClass dv dt tySynTvbs (RecC n vbts) = do
    let (selNames, bangs, _) = unzip3 vbts
    ssis <- reifySelStrictInfo n bangs
    repConWith gClass dv dt n tySynTvbs (Just selNames) ssis True False
repCon gClass dv dt tySynTvbs (InfixC t1 n t2) = do
    let bangs = map fst [t1, t2]
    ssis <- reifySelStrictInfo n bangs
    repConWith gClass dv dt n tySynTvbs Nothing ssis False True
repCon _ _ _ _ con = gadtError con

repConWith :: GenericClass
           -> DataVariety
           -> Name
           -> Name
           -> [TyVarBndr]
           -> Maybe [Name]
           -> [SelStrictInfo]
           -> Bool
           -> Bool
           -> Q Type
repConWith gClass dv dt n tySynTvbs mbSelNames ssis isRecord isInfix = do
    (conTvbs, ts, gk) <- reifyConTys gClass n

    let structureType :: Q Type
        structureType = case ssis of
                             [] -> conT u1TypeName
                             _  -> foldr1 prodT f

        -- See Note [Substituting types in a constructor type signature]
        typeSubst :: TypeSubst
        typeSubst = Map.fromList $
          zip (concatMap             tyVarNamesOfTyVarBndr  conTvbs)
              (concatMap (map VarT . tyVarNamesOfTyVarBndr) tySynTvbs)

        f :: [Q Type]
        f = case mbSelNames of
                 Just selNames -> zipWith3 (repField gk dv dt n typeSubst . Just)
                                           selNames ssis ts
                 Nothing       -> zipWith  (repField gk dv dt n typeSubst Nothing)
                                           ssis ts

    conT c1TypeName
      `appT` mkMetaConsType dv dt n isRecord isInfix
      `appT` structureType

prodT :: Q Type -> Q Type -> Q Type
prodT a b = conT productTypeName `appT` a `appT` b

repField :: GenericKind
         -> DataVariety
         -> Name
         -> Name
         -> TypeSubst
         -> Maybe Name
         -> SelStrictInfo
         -> Type
         -> Q Type
repField gk dv dt ns typeSubst mbF ssi t =
           conT s1TypeName
    `appT` mkMetaSelType dv dt ns mbF ssi
    `appT` (repFieldArg gk =<< expandSyn t'')
  where
    -- See Note [Substituting types in constructor type signatures]
    t', t'' :: Type
    t' = case gk of
              Gen1 _ (Just kvName) -> substNameWithKind kvName starK t
              _ -> t
    t'' = substType typeSubst t'

repFieldArg :: GenericKind -> Type -> Q Type
repFieldArg _ ForallT{} = rankNError
repFieldArg gk (SigT t _) = repFieldArg gk t
repFieldArg Gen0 t = boxT t
repFieldArg (Gen1 name _) (VarT t) | t == name = conT par1TypeName
repFieldArg gk@(Gen1 name _) t = do
  let tyHead:tyArgs      = unapplyTy t
      numLastArgs        = min 1 $ length tyArgs
      (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs
      rec0Type           = boxT t
      phiType            = return $ applyTyToTys tyHead lhsArgs

      inspectTy :: Type -> Q Type
      inspectTy (VarT a)
        | a == name
        = conT rec1TypeName `appT` phiType
      inspectTy (SigT ty _) = inspectTy ty
      inspectTy beta
        | not (ground beta name)
        = conT composeTypeName `appT` phiType
                               `appT` repFieldArg gk beta
      inspectTy _ = rec0Type

  itf <- isTyFamily tyHead
  if any (not . (`ground` name)) lhsArgs
       || any (not . (`ground` name)) tyArgs && itf
     then outOfPlaceTyVarError
     else case rhsArgs of
          []   -> rec0Type
          ty:_ -> inspectTy ty

boxT :: Type -> Q Type
boxT ty = case unboxedRepNames ty of
    Just (boxTyName, _, _) -> conT boxTyName
    Nothing                -> conT rec0TypeName `appT` return ty

mkCaseExp :: GenericClass -> Name -> [Con]
          -> (GenericClass -> Int -> Int -> Name -> [Con] -> Q Match)
          -> Q Exp
mkCaseExp gClass dt cs matchmaker = do
  val <- newName "val"
  lam1E (varP val) $ caseE (varE val) [matchmaker gClass 1 0 dt cs]

mkFrom :: GenericClass -> Int -> Int -> Name -> [Con] -> Q Match
mkFrom gClass m i dt cs = do
    y <- newName "y"
    match (varP y)
          (normalB $ conE m1DataName `appE` caseE (varE y) cases)
          []
  where
    cases = case cs of
              [] -> [errorFrom dt]
              _  -> zipWith (fromCon gClass wrapE (length cs)) [0..] cs
    wrapE e = lrE m i e

errorFrom :: Name -> Q Match
errorFrom dt =
  match
    wildP
    (normalB $ varE errorValName `appE` stringE
      ("No generic representation for empty datatype " ++ nameBase dt))
    []

errorTo :: Name -> Q Match
errorTo dt =
  match
    wildP
    (normalB $ varE errorValName `appE` stringE
      ("No values for empty datatype " ++ nameBase dt))
    []

mkTo :: GenericClass -> Int -> Int -> Name -> [Con] -> Q Match
mkTo gClass m i dt cs = do
    y <- newName "y"
    match (conP m1DataName [varP y])
          (normalB $ caseE (varE y) cases)
          []
  where
    cases = case cs of
              [] -> [errorTo dt]
              _  -> zipWith (toCon gClass wrapP (length cs)) [0..] cs
    wrapP p = lrP m i p

fromCon :: GenericClass -> (Q Exp -> Q Exp) -> Int -> Int -> Con -> Q Match
fromCon _ wrap m i (NormalC cn []) =
  match
    (conP cn [])
    (normalB $ wrap $ lrE m i $ conE m1DataName `appE` (conE u1DataName)) []
fromCon gClass wrap m i (NormalC cn _) = do
  (ts, gk) <- fmap shrink $ reifyConTys gClass cn
  fNames   <- newNameList "f" $ length ts
  match
    (conP cn (map varP fNames))
    (normalB $ wrap $ lrE m i $ conE m1DataName `appE`
      foldr1 prodE (zipWith (fromField gk) fNames ts)) []
fromCon _ wrap m i (RecC cn []) =
  match
    (conP cn [])
    (normalB $ wrap $ lrE m i $ conE m1DataName `appE` (conE u1DataName)) []
fromCon gClass wrap m i (RecC cn _) = do
  (ts, gk) <- fmap shrink $ reifyConTys gClass cn
  fNames   <- newNameList "f" $ length ts
  match
    (conP cn (map varP fNames))
    (normalB $ wrap $ lrE m i $ conE m1DataName `appE`
      foldr1 prodE (zipWith (fromField gk) fNames ts)) []
fromCon gClass wrap m i (InfixC t1 cn t2) =
  fromCon gClass wrap m i (NormalC cn [t1,t2])
fromCon _ _ _ _ con = gadtError con

prodE :: Q Exp -> Q Exp -> Q Exp
prodE x y = conE productDataName `appE` x `appE` y

fromField :: GenericKind -> Name -> Type -> Q Exp
fromField gk nr t = conE m1DataName `appE` (fromFieldWrap gk nr =<< expandSyn t)

fromFieldWrap :: GenericKind -> Name -> Type -> Q Exp
fromFieldWrap _             _  ForallT{}  = rankNError
fromFieldWrap gk            nr (SigT t _) = fromFieldWrap gk nr t
fromFieldWrap Gen0          nr t          = conE (boxRepName t) `appE` varE nr
fromFieldWrap (Gen1 name _) nr t          = wC t name           `appE` varE nr

wC :: Type -> Name -> Q Exp
wC (VarT t) name | t == name = conE par1DataName
wC t name
  | ground t name = conE $ boxRepName t
  | otherwise = do
      let tyHead:tyArgs      = unapplyTy t
          numLastArgs        = min 1 $ length tyArgs
          (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

          inspectTy :: Type -> Q Exp
          inspectTy ForallT{} = rankNError
          inspectTy (SigT ty _) = inspectTy ty
          inspectTy (VarT a)
            | a == name
            = conE rec1DataName
          inspectTy beta = infixApp (conE comp1DataName)
                                    (varE composeValName)
                                    (varE fmapValName `appE` wC beta name)

      itf <- isTyFamily tyHead
      if any (not . (`ground` name)) lhsArgs
           || any (not . (`ground` name)) tyArgs && itf
         then outOfPlaceTyVarError
         else case rhsArgs of
              []   -> conE $ boxRepName t
              ty:_ -> inspectTy ty

boxRepName :: Type -> Name
boxRepName = maybe k1DataName snd3 . unboxedRepNames

toCon :: GenericClass -> (Q Pat -> Q Pat) -> Int -> Int -> Con -> Q Match
toCon _ wrap m i (NormalC cn []) =
    match
      (wrap $ lrP m i $ conP m1DataName [conP u1DataName []])
      (normalB $ conE cn) []
toCon gClass wrap m i (NormalC cn _) = do
    (ts, gk) <- fmap shrink $ reifyConTys gClass cn
    fNames   <- newNameList "f" $ length ts
    match
      (wrap $ lrP m i $ conP m1DataName
        [foldr1 prod (zipWith (toField gk) fNames ts)])
      (normalB $ foldl appE (conE cn) (zipWith (\nr -> expandSyn >=> toConUnwC gk nr)
                                      fNames ts)) []
  where prod x y = conP productDataName [x,y]
toCon _ wrap m i (RecC cn []) =
    match
      (wrap $ lrP m i $ conP m1DataName [conP u1DataName []])
      (normalB $ conE cn) []
toCon gClass wrap m i (RecC cn _) = do
    (ts, gk) <- fmap shrink $ reifyConTys gClass cn
    fNames   <- newNameList "f" $ length ts
    match
      (wrap $ lrP m i $ conP m1DataName
        [foldr1 prod (zipWith (toField gk) fNames ts)])
      (normalB $ foldl appE (conE cn) (zipWith (\nr -> expandSyn >=> toConUnwC gk nr)
                                               fNames ts)) []
  where prod x y = conP productDataName [x,y]
toCon gk wrap m i (InfixC t1 cn t2) =
  toCon gk wrap m i (NormalC cn [t1,t2])
toCon _ _ _ _ con = gadtError con

toConUnwC :: GenericKind -> Name -> Type -> Q Exp
toConUnwC Gen0          nr _ = varE nr
toConUnwC (Gen1 name _) nr t = unwC t name `appE` varE nr

toField :: GenericKind -> Name -> Type -> Q Pat
toField gk nr t = conP m1DataName [toFieldWrap gk nr t]

toFieldWrap :: GenericKind -> Name -> Type -> Q Pat
toFieldWrap Gen0   nr t = conP (boxRepName t) [varP nr]
toFieldWrap Gen1{} nr _ = varP nr

unwC :: Type -> Name -> Q Exp
unwC (SigT t _) name = unwC t name
unwC (VarT t)   name | t == name = varE unPar1ValName
unwC t name
  | ground t name = varE $ unboxRepName t
  | otherwise = do
      let tyHead:tyArgs      = unapplyTy t
          numLastArgs        = min 1 $ length tyArgs
          (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

          inspectTy :: Type -> Q Exp
          inspectTy ForallT{} = rankNError
          inspectTy (SigT ty _) = inspectTy ty
          inspectTy (VarT a)
            | a == name
            = varE unRec1ValName
          inspectTy beta = infixApp (varE fmapValName `appE` unwC beta name)
                                    (varE composeValName)
                                    (varE unComp1ValName)

      itf <- isTyFamily tyHead
      if any (not . (`ground` name)) lhsArgs
           || any (not . (`ground` name)) tyArgs && itf
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

-- | Deduces the instance type to use for a Generic(1) instance.
buildTypeInstance :: GenericClass
                  -- ^ Generic or Generic1
                  -> Name
                  -- ^ The type constructor or data family name
                  -> [TyVarBndr]
                  -- ^ The type variables from the data type/data family declaration
                  -> DataVariety
                  -- ^ If using a data family instance, provides the types used
                  -- to instantiate the instance
                  -> Q Type
-- Plain data type/newtype case
buildTypeInstance gClass tyConName tvbs DataPlain =
    let varTys :: [Type]
        varTys = map tyVarBndrToType tvbs
    in buildTypeInstanceFromTys gClass tyConName varTys False
-- Data family instance case
--
-- The CPP is present to work around a couple of annoying old GHC bugs.
-- See Note [Polykinded data families in Template Haskell]
buildTypeInstance gClass parentName tvbs (DataFamily _ instTysAndKinds) = do
#if !(MIN_VERSION_template_haskell(2,8,0)) || MIN_VERSION_template_haskell(2,10,0)
    let instTys :: [Type]
        instTys = zipWith stealKindForType tvbs instTysAndKinds
#else
    let kindVarNames :: [Name]
        kindVarNames = nub $ concatMap (tyVarNamesOfType . tyVarBndrKind) tvbs

        numKindVars :: Int
        numKindVars = length kindVarNames

        givenKinds, givenKinds' :: [Kind]
        givenTys                :: [Type]
        (givenKinds, givenTys) = splitAt numKindVars instTysAndKinds
        givenKinds' = map sanitizeStars givenKinds

        -- A GHC 7.6-specific bug requires us to replace all occurrences of
        -- (ConT GHC.Prim.*) with StarT, or else Template Haskell will reject it.
        -- Luckily, (ConT GHC.Prim.*) only seems to occur in this one spot.
        sanitizeStars :: Kind -> Kind
        sanitizeStars = go
          where
            go :: Kind -> Kind
            go (AppT t1 t2)                 = AppT (go t1) (go t2)
            go (SigT t k)                   = SigT (go t) (go k)
            go (ConT n) | n == starKindName = StarT
            go t                            = t

    -- If we run this code with GHC 7.8, we might have to generate extra type
    -- variables to compensate for any type variables that Template Haskell
    -- eta-reduced away.
    -- See Note [Polykinded data families in Template Haskell]
    xTypeNames <- newNameList "tExtra" (length tvbs - length givenTys)

    let xTys   :: [Type]
        xTys = map VarT xTypeNames
        -- ^ Because these type variables were eta-reduced away, we can only
        --   determine their kind by using stealKindForType. Therefore, we mark
        --   them as VarT to ensure they will be given an explicit kind annotation
        --   (and so the kind inference machinery has the right information).

        substNamesWithKinds :: [(Name, Kind)] -> Type -> Type
        substNamesWithKinds nks t = foldr' (uncurry substNameWithKind) t nks

        -- The types from the data family instance might not have explicit kind
        -- annotations, which the kind machinery needs to work correctly. To
        -- compensate, we use stealKindForType to explicitly annotate any
        -- types without kind annotations.
        instTys :: [Type]
        instTys = map (substNamesWithKinds (zip kindVarNames givenKinds'))
                  -- Note that due to a GHC 7.8-specific bug
                  -- (see Note [Polykinded data families in Template Haskell]),
                  -- there may be more kind variable names than there are kinds
                  -- to substitute. But this is OK! If a kind is eta-reduced, it
                  -- means that is was not instantiated to something more specific,
                  -- so we need not substitute it. Using stealKindForType will
                  -- grab the correct kind.
                $ zipWith stealKindForType tvbs (givenTys ++ xTys)
#endif
    buildTypeInstanceFromTys gClass parentName instTys True

-- For the given Types, deduces the instance type to use for a Generic(1) instance.
-- Coming up with the instance type isn't as simple as dropping the last types, as
-- you need to be wary of kinds being instantiated with *.
-- See Note [Type inference in derived instances]
buildTypeInstanceFromTys :: GenericClass
                         -- ^ Generic or Generic1
                         -> Name
                         -- ^ The type constructor or data family name
                         -> [Type]
                         -- ^ The types to instantiate the instance with
                         -> Bool
                         -- ^ True if it's a data family, False otherwise
                         -> Q Type
buildTypeInstanceFromTys gClass tyConName varTysOrig isDataFamily = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM expandSyn varTysOrig

    let remainingLength :: Int
        remainingLength = length varTysOrig - fromEnum gClass

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    -- Check there are enough types to drop and that all of them are either of
    -- kind * or kind k (for some kind variable k). If not, throw an error.
    when (remainingLength < 0 || any (== NotKindStar) droppedStarKindStati) $
      derivingKindError tyConName

    let droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati

        -- Substitute kind * for any dropped kind variables
        varTysExpSubst :: [Type]
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

    -- We must take care to avoid allowing Generic1 instances where a visible kind
    -- binder is instantiated to * (which should only happen in the presence of
    -- -XTypeInType). See the documentation for instantiationError for an example
    -- of when this can occur.
    --
    -- A quick-and-dirty way to accomplish this is to check if the visible type
    -- binders of the original type, and of the type post-synonym-expansion, are
    -- both the same. If not, it's likely that one of the type binders was
    -- instantiated to a specific type (likely *).
    when (concatMap visibleTyVarsOfType varTysExp
            /= concatMap visibleTyVarsOfType varTysExpSubst) $
      instantiationError tyConName

    let remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

    -- If any of the dropped types were polykinded, ensure that there are of
    -- kind * after substituting * for the dropped kind variables. If not,
    -- throw an error.
    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError tyConName

        -- We now substitute all of the specialized-to-* kind variable names
        -- with *, but in the original types, not the synonym-expanded types. The reason
        -- we do this is a superficial one: we want the derived instance to resemble
        -- the datatype written in source code as closely as possible. For example,
        -- for the following data family instance:
        --
        --   data family Fam a
        --   newtype instance Fam String = Fam String
        --
        -- We'd want to generate the instance:
        --
        --   instance C (Fam String)
        --
        -- Not:
        --
        --   instance C (Fam [Char])
    let remainingTysOrigSubst :: [Type]
        remainingTysOrigSubst =
          map (substNamesWithKindStar droppedKindVarNames)
            $ take remainingLength varTysOrig

        remainingTysOrigSubst' :: [Type]
        -- See Note [Kind signatures in derived instances] for an explanation
        -- of the isDataFamily check.
        remainingTysOrigSubst' =
          if isDataFamily
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceType :: Type
        instanceType = applyTyToTys (ConT tyConName) remainingTysOrigSubst'

    -- Ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst droppedTysExpSubst) $
      etaReductionError instanceType
    return instanceType

-- See Note [Arguments to generated type synonyms]
grabTyVarBndrsFromCons :: GenericClass -> [Con] -> Q [TyVarBndr]
grabTyVarBndrsFromCons _      []      = return []
grabTyVarBndrsFromCons gClass (con:_) =
    fmap fst3 $ reifyConTys gClass (constructorName con)

{-
Note [Forcing buildTypeInstance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sometimes, we don't explicitly need to generate a Generic(1) type instance, but
we force buildTypeInstance nevertheless. This is because it performs some checks
for whether or not the provided datatype can actually have Generic(1) implemented for
it, and produces errors if it can't. Otherwise, laziness would cause these checks
to be skipped entirely, which could result in some indecipherable type errors
down the road.

Note [Substituting types in constructor type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While reifyConTys gives you the type variables of a constructor, they may not be
the same of the data declaration's type variables. The classic example of this is
GADTs:

  data GADT a b where
    GADTCon :: e -> f -> GADT e f

The type variables of GADTCon are completely different from the declaration's, which
can cause a problem when generating a Rep instance:

  type Rep (GADT a b) = Rec0 e :*: Rec0 f

Naïvely, we would generate something like this, since traversing the constructor
would give us precisely those arguments. Not good. We need to perform a type
substitution to ensure that e maps to a, and f maps to b.

This turns out to be surprisingly simple. Whenever you have a constructor type
signature like (e -> f -> GADT e f), take the result type, collect all of its
distinct type variables in order from left-to-right, and then map them to their
corresponding type variables from the data declaration.

There is another obscure case where we need to do a type subtitution. With
-XTypeInType enabled, you might have something like this:

  data Proxy (a :: k) (b :: k) = Proxy k deriving Generic1

Then k gets specialized to *, which means that k should NOT show up in the RHS of
a Rep1 type instance! To avoid this, make sure to substitute k with *.

Note [Arguments to generated type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A surprisingly difficult component of generating the type synonyms for Rep/Rep1 is
coming up with the type synonym variable arguments, since they have to be just the
right name and kind to work. The type signature of a constructor does a remarkably
good job of coming up with these type variables for us, so if at least one constructor
exists, we simply steal the type variables from that constructor's type signature
for use in the generated type synonym. We also count the number of type variables
that the first constructor's type signature has in order to determine how many
type variables we should give it as arguments in the generated
(type Rep (Foo ...) = <makeRep> ...) code.

This leads one to ask: what if there are no constructors? If that's the case, then
we're OK, since that means no type variables can possibly appear on the RHS of the
type synonym! In such a special case, we're perfectly justified in making the type
synonym not have any type variable arguments, and similarly, we don't apply any
arguments to it in the generated (type Rep Foo = <makeRep>) code.

Note [Polykinded data families in Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to come up with the correct instance context and head for an instance, e.g.,

  instance C a => C (Data a) where ...

We need to know the exact types and kinds used to instantiate the instance. For
plain old datatypes, this is simple: every type must be a type variable, and
Template Haskell reliably tells us the type variables and their kinds.

Doing the same for data families proves to be much harder for three reasons:

1. On any version of Template Haskell, it may not tell you what an instantiated
   type's kind is. For instance, in the following data family instance:

     data family Fam (f :: * -> *) (a :: *)
     data instance Fam f a

   Then if we use TH's reify function, it would tell us the TyVarBndrs of the
   data family declaration are:

     [KindedTV f (AppT (AppT ArrowT StarT) StarT),KindedTV a StarT]

   and the instantiated types of the data family instance are:

     [VarT f1,VarT a1]

   We can't just pass [VarT f1,VarT a1] to buildTypeInstanceFromTys, since we
   have no way of knowing their kinds. Luckily, the TyVarBndrs tell us what the
   kind is in case an instantiated type isn't a SigT, so we use the stealKindForType
   function to ensure all of the instantiated types are SigTs before passing them
   to buildTypeInstanceFromTys.
2. On GHC 7.6 and 7.8, a bug is present in which Template Haskell lists all of
   the specified kinds of a data family instance efore any of the instantiated
   types. Fortunately, this is easy to deal with: you simply count the number of
   distinct kind variables in the data family declaration, take that many elements
   from the front of the  Types list of the data family instance, substitute the
   kind variables with their respective instantiated kinds (which you took earlier),
   and proceed as normal.
3. On GHC 7.8, an even uglier bug is present (GHC Trac #9692) in which Template
   Haskell might not even list all of the Types of a data family instance, since
   they are eta-reduced away! And yes, kinds can be eta-reduced too.

   The simplest workaround is to count how many instantiated types are missing from
   the list and generate extra type variables to use in their place. Luckily, we
   needn't worry much if its kind was eta-reduced away, since using stealKindForType
   will get it back.

Note [Kind signatures in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to put explicit kind signatures into the derived instances, e.g.,

  instance C a => C (Data (f :: * -> *)) where ...

But it is preferable to avoid this if possible. If we come up with an incorrect
kind signature (which is entirely possible, since our type inferencer is pretty
unsophisticated - see Note [Type inference in derived instances]), then GHC will
flat-out reject the instance, which is quite unfortunate.

Plain old datatypes have the advantage that you can avoid using any kind signatures
at all in their instances. This is because a datatype declaration uses all type
variables, so the types that we use in a derived instance uniquely determine their
kinds. As long as we plug in the right types, the kind inferencer can do the rest
of the work. For this reason, we use unSigT to remove all kind signatures before
splicing in the instance context and head.

Data family instances are trickier, since a data family can have two instances that
are distinguished by kind alone, e.g.,

  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)

If we dropped the kind signatures for C (Fam a), then GHC will have no way of
knowing which instance we are talking about. To avoid this scenario, we always
include explicit kind signatures in data family instances. There is a chance that
the inferred kind signatures will be incorrect, but if so, we can always fall back
on the make- functions.

-}
