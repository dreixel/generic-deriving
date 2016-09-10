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
      -- * @derive@- functions
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

     -- * @make@- functions
     -- $make
    , makeRep0Inline
    , makeRep0
    , makeRep0FromType
    , makeFrom
    , makeFrom0
    , makeTo
    , makeTo0
    , makeRep1Inline
    , makeRep1
    , makeRep1FromType
    , makeFrom1
    , makeTo1

     -- * Options
     -- $options
     -- ** Option types
    , Options(..)
    , defaultOptions
    , RepOptions(..)
    , defaultRepOptions
    , KindSigOptions
    , defaultKindSigOptions

    -- ** Functions with optional arguments
    , deriveAll0Options
    , deriveAll1Options
    , deriveAll0And1Options
    , deriveRepresentable0Options
    , deriveRepresentable1Options
    , deriveRep0Options
    , deriveRep1Options
  ) where

import           Control.Monad ((>=>), unless, when)

#if MIN_VERSION_template_haskell(2,8,0) && !(MIN_VERSION_template_haskell(2,10,0))
import           Data.Foldable (foldr')
#endif
import           Data.List (nub)
import qualified Data.Map as Map (fromList)
import           Data.Maybe (catMaybes)

import           Generics.Deriving.TH.Internal
#if MIN_VERSION_base(4,9,0)
import           Generics.Deriving.TH.Post4_9
#else
import           Generics.Deriving.TH.Pre4_9
#endif

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH

{- $options
'Options' gives you a way to further tweak derived 'Generic' and 'Generic1' instances:

* 'RepOptions': By default, all derived 'Rep' and 'Rep1' type instances emit the code
  directly (the 'InlineRep' option). One can also choose to emit a separate type
  synonym for the 'Rep' type (this is the functionality of 'deriveRep0' and
  'deriveRep1') and define a 'Rep' instance in terms of that type synonym (the
  'TypeSynonymRep' option).

* 'KindSigOptions': By default, all derived instances will use explicit kind
  signatures (when the 'KindSigOptions' is 'True'). You might wish to set the
  'KindSigOptions' to 'False' if you want a 'Generic'/'Generic1' instance at
  a particular kind that GHC will infer correctly, but the functions in this
  module won't guess correctly. For example, the following example will only
  compile with 'KindSigOptions' set to 'False':

  @
  newtype Compose (f :: k2 -> *) (g :: k1 -> k2) (a :: k1) = Compose (f (g a))
  $('deriveAll1Options' False ''Compose)
  @
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

-- | Additional options for configuring derived 'Generic'/'Generic1' instances
-- using Template Haskell.
data Options = Options
  { repOptions     :: RepOptions
  , kindSigOptions :: KindSigOptions
  } deriving (Eq, Ord, Read, Show)

-- | Sensible default 'Options' ('defaultRepOptions' and 'defaultKindSigOptions').
defaultOptions :: Options
defaultOptions = Options
  { repOptions     = defaultRepOptions
  , kindSigOptions = defaultKindSigOptions
  }

-- | Configures whether 'Rep'/'Rep1' type instances should be defined inline in a
-- derived 'Generic'/'Generic1' instance ('InlineRep') or defined in terms of a
-- type synonym ('TypeSynonymRep').
data RepOptions = InlineRep
                | TypeSynonymRep
  deriving (Eq, Ord, Read, Show)

-- | 'InlineRep', a sensible default 'RepOptions'.
defaultRepOptions :: RepOptions
defaultRepOptions = InlineRep

-- | 'True' if explicit kind signatures should be used in derived
-- 'Generic'/'Generic1' instances, 'False' otherwise.
type KindSigOptions = Bool

-- | 'True', a sensible default 'KindSigOptions'.
defaultKindSigOptions :: KindSigOptions
defaultKindSigOptions = True

-- | A backwards-compatible synonym for 'deriveAll0'.
deriveAll :: Name -> Q [Dec]
deriveAll = deriveAll0

-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, the 'Selector'
-- instances, and the 'Representable0' instance.
deriveAll0 :: Name -> Q [Dec]
deriveAll0 = deriveAll0Options defaultOptions

-- | Like 'deriveAll0', but takes an 'Options' argument.
deriveAll0Options :: Options -> Name -> Q [Dec]
deriveAll0Options = deriveAllCommon True False

-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, the 'Selector'
-- instances, and the 'Representable1' instance.
deriveAll1 :: Name -> Q [Dec]
deriveAll1 = deriveAll1Options defaultOptions

-- | Like 'deriveAll1', but takes an 'Options' argument.
deriveAll1Options :: Options -> Name -> Q [Dec]
deriveAll1Options = deriveAllCommon False True

-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, the 'Selector'
-- instances, the 'Representable0' instance, and the 'Representable1' instance.
deriveAll0And1 :: Name -> Q [Dec]
deriveAll0And1 = deriveAll0And1Options defaultOptions

-- | Like 'deriveAll0And1', but takes an 'Options' argument.
deriveAll0And1Options :: Options -> Name -> Q [Dec]
deriveAll0And1Options = deriveAllCommon True True

deriveAllCommon :: Bool -> Bool -> Options -> Name -> Q [Dec]
deriveAllCommon generic generic1 opts n = do
    a <- deriveMeta n
    b <- if generic
            then deriveRepresentableCommon Generic opts n
            else return []
    c <- if generic1
            then deriveRepresentableCommon Generic1 opts n
            else return []
    return (a ++ b ++ c)

-- | Given the type and the name (as string) for the Representable0 type
-- synonym to derive, generate the 'Representable0' instance.
deriveRepresentable0 :: Name -> Q [Dec]
deriveRepresentable0 = deriveRepresentable0Options defaultOptions

-- | Like 'deriveRepresentable0', but takes an 'Options' argument.
deriveRepresentable0Options :: Options -> Name -> Q [Dec]
deriveRepresentable0Options = deriveRepresentableCommon Generic

-- | Given the type and the name (as string) for the Representable1 type
-- synonym to derive, generate the 'Representable1' instance.
deriveRepresentable1 :: Name -> Q [Dec]
deriveRepresentable1 = deriveRepresentable1Options defaultOptions

-- | Like 'deriveRepresentable1', but takes an 'Options' argument.
deriveRepresentable1Options :: Options -> Name -> Q [Dec]
deriveRepresentable1Options = deriveRepresentableCommon Generic1

deriveRepresentableCommon :: GenericClass -> Options -> Name -> Q [Dec]
deriveRepresentableCommon gClass opts n = do
    rep  <- if repOptions opts == InlineRep
               then return []
               else deriveRepCommon gClass (kindSigOptions opts) n
    inst <- deriveInst gClass opts n
    return (rep ++ inst)

-- | Derive only the 'Rep0' type synonym. Not needed if 'deriveRepresentable0'
-- is used.
deriveRep0 :: Name -> Q [Dec]
deriveRep0 = deriveRep0Options defaultKindSigOptions

-- | Like 'deriveRep0', but takes an 'KindSigOptions' argument.
deriveRep0Options :: KindSigOptions -> Name -> Q [Dec]
deriveRep0Options = deriveRepCommon Generic

-- | Derive only the 'Rep1' type synonym. Not needed if 'deriveRepresentable1'
-- is used.
deriveRep1 :: Name -> Q [Dec]
deriveRep1 = deriveRep1Options defaultKindSigOptions

-- | Like 'deriveRep1', but takes an 'KindSigOptions' argument.
deriveRep1Options :: KindSigOptions -> Name -> Q [Dec]
deriveRep1Options = deriveRepCommon Generic1

deriveRepCommon :: GenericClass -> KindSigOptions -> Name -> Q [Dec]
deriveRepCommon gClass useKindSigs n = do
  i <- reifyDataInfo n
  let (name, isNT, declTvbs, cons, dv) = either error id i
  -- See Note [Forcing buildTypeInstance]
  !_ <- buildTypeInstance gClass useKindSigs name declTvbs dv
  tySynVars <- grabTyVarsFromCons gClass cons

  -- See Note [Kind signatures in derived instances]
  let tySynVars' = if useKindSigs
                      then tySynVars
                      else map unSigT tySynVars
  fmap (:[]) $ tySynD (genRepName gClass dv name)
                      (catMaybes $ map typeToTyVarBndr tySynVars')
                      (repType gClass dv name isNT cons tySynVars)

deriveInst :: GenericClass -> Options -> Name -> Q [Dec]
deriveInst Generic  = deriveInstCommon genericTypeName  repTypeName  Generic  fromValName  toValName
deriveInst Generic1 = deriveInstCommon generic1TypeName rep1TypeName Generic1 from1ValName to1ValName

deriveInstCommon :: Name
                 -> Name
                 -> GenericClass
                 -> Name
                 -> Name
                 -> Options
                 -> Name
                 -> Q [Dec]
deriveInstCommon genericName repName gClass fromName toName opts n = do
  i <- reifyDataInfo n
  let (name, isNT, allTvbs, cons, dv) = either error id i
      useKindSigs = kindSigOptions opts
  -- See Note [Forcing buildTypeInstance]
  !(origTy, origKind) <- buildTypeInstance gClass useKindSigs name allTvbs dv
  tyInsRHS <- if repOptions opts == InlineRep
                 then makeRepInline   gClass dv name isNT cons origTy
                 else makeRepTySynApp gClass dv name      cons origTy

  let origSigTy = if useKindSigs
                     then SigT origTy origKind
                     else origTy
      tyIns = TySynInstD repName
#if MIN_VERSION_template_haskell(2,9,0)
                         (TySynEqn [origSigTy] tyInsRHS)
#else
                         [origSigTy] tyInsRHS
#endif
      mkBody maker = [clause [] (normalB $ mkCaseExp gClass name cons maker) []]
      fcs = mkBody mkFrom
      tcs = mkBody mkTo

  fmap (:[]) $
    instanceD (cxt []) (conT genericName `appT` return origSigTy)
                         [return tyIns, funD fromName fcs, funD toName tcs]

{- $make

There are some data types for which the Template Haskell deriver functions in
this module are not sophisticated enough to infer the correct 'Generic' or
'Generic1' instances. As an example, consider this data type:

@
newtype Fix f a = Fix (f (Fix f a))
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
  type Rep1 (Fix f) = $('makeRep1Inline' ''Fix [t| Fix f |])
  from1 = $('makeFrom1' ''Fix)
  to1   = $('makeTo1'   ''Fix)
@

Note that due to the lack of type-level lambdas in Haskell, one must manually
apply @'makeRep1Inline' ''Fix@ to the type @Fix f@.

Be aware that there is a bug on GHC 7.0, 7.2, and 7.4 which might prevent you from
using 'makeRep0Inline' and 'makeRep1Inline'. In the @Fix@ example above, you
would experience the following error:

@
    Kinded thing `f' used as a type
    In the Template Haskell quotation [t| Fix f |]
@

Then a workaround is to use 'makeRep1' instead, which requires you to:

1. Invoke 'deriveRep1' beforehand

2. Pass as arguments the type variables that occur in the instance, in order
   from left to right, topologically sorted, excluding duplicates. (Normally,
   'makeRep1Inline' would figure this out for you.)

Using the above example:

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

-- | Generates the full 'Rep' type inline. Since this type can be quite
-- large, it is recommended you only use this to define 'Rep', e.g.,
--
-- @
-- type Rep (Foo (a :: k) b) = $('makeRep0Inline' ''Foo [t| Foo (a :: k) b |])
-- @
--
-- You can then simply refer to @Rep (Foo a b)@ elsewhere.
--
-- Note that the type passed as an argument to 'makeRep0Inline' must match the
-- type argument of 'Rep' exactly, even up to including the explicit kind
-- signature on @a@. This is due to a limitation of Template Haskell—without
-- the kind signature, 'makeRep0Inline' has no way of figuring out the kind of
-- @a@, and the generated type might be completely wrong as a result!
makeRep0Inline :: Name -> Q Type -> Q Type
makeRep0Inline n = makeRepCommon Generic InlineRep n . Just

-- | Generates the full 'Rep1' type inline. Since this type can be quite
-- large, it is recommended you only use this to define 'Rep1', e.g.,
--
-- @
-- type Rep1 (Foo (a :: k)) = $('makeRep0Inline' ''Foo [t| Foo (a :: k) |])
-- @
--
-- You can then simply refer to @Rep1 (Foo a)@ elsewhere.
--
-- Note that the type passed as an argument to 'makeRep1Inline' must match the
-- type argument of 'Rep1' exactly, even up to including the explicit kind
-- signature on @a@. This is due to a limitation of Template Haskell—without
-- the kind signature, 'makeRep1Inline' has no way of figuring out the kind of
-- @a@, and the generated type might be completely wrong as a result!
makeRep1Inline :: Name -> Q Type -> Q Type
makeRep1Inline n = makeRepCommon Generic1 InlineRep n . Just

-- | Generates the 'Rep' type synonym constructor (as opposed to 'deriveRep0',
-- which generates the type synonym declaration). After splicing it into
-- Haskell source, it expects types as arguments. For example:
--
-- @
-- type Rep (Foo a b) = $('makeRep0' ''Foo) a b
-- @
--
-- The use of 'makeRep0' is generally discouraged, as it can sometimes be
-- difficult to predict the order in which you are expected to pass type
-- variables. As a result, 'makeRep0Inline' is recommended instead. However,
-- 'makeRep0Inline' is not usable on GHC 7.0, 7.2, or 7.4 due to a GHC bug,
-- so 'makeRep0' still exists for GHC 7.0, 7.2, and 7.4 users.
makeRep0 :: Name -> Q Type
makeRep0 n = makeRepCommon Generic TypeSynonymRep n Nothing

-- | Generates the 'Rep1' type synonym constructor (as opposed to 'deriveRep1',
-- which generates the type synonym declaration). After splicing it into
-- Haskell source, it expects types as arguments. For example:
--
-- @
-- type Rep1 (Foo a) = $('makeRep1' ''Foo) a
-- @
--
-- The use of 'makeRep1' is generally discouraged, as it can sometimes be
-- difficult to predict the order in which you are expected to pass type
-- variables. As a result, 'makeRep1Inline' is recommended instead. However,
-- 'makeRep1Inline' is not usable on GHC 7.0, 7.2, or 7.4 due to a GHC bug,
-- so 'makeRep1' still exists for GHC 7.0, 7.2, and 7.4 users.
makeRep1 :: Name -> Q Type
makeRep1 n = makeRepCommon Generic1 TypeSynonymRep n Nothing

-- | Generates the 'Rep' type synonym constructor (as opposed to 'deriveRep0',
-- which generates the type synonym declaration) applied to its type arguments.
-- Unlike 'makeRep0', this also takes a quoted 'Type' as an argument, e.g.,
--
-- @
-- type Rep (Foo (a :: k) b) = $('makeRep0FromType' ''Foo [t| Foo (a :: k) b |])
-- @
--
-- Note that the type passed as an argument to 'makeRep0FromType' must match the
-- type argument of 'Rep' exactly, even up to including the explicit kind
-- signature on @a@. This is due to a limitation of Template Haskell—without
-- the kind signature, 'makeRep0FromType' has no way of figuring out the kind of
-- @a@, and the generated type might be completely wrong as a result!
--
-- The use of 'makeRep0FromType' is generally discouraged, since 'makeRep0Inline'
-- does exactly the same thing but without having to go through an intermediate
-- type synonym, and as a result, 'makeRep0Inline' tends to be less buggy.
makeRep0FromType :: Name -> Q Type -> Q Type
makeRep0FromType n = makeRepCommon Generic TypeSynonymRep n . Just

-- | Generates the 'Rep1' type synonym constructor (as opposed to 'deriveRep1',
-- which generates the type synonym declaration) applied to its type arguments.
-- Unlike 'makeRep1', this also takes a quoted 'Type' as an argument, e.g.,
--
-- @
-- type Rep1 (Foo (a :: k)) = $('makeRep1FromType' ''Foo [t| Foo (a :: k) |])
-- @
--
-- Note that the type passed as an argument to 'makeRep1FromType' must match the
-- type argument of 'Rep' exactly, even up to including the explicit kind
-- signature on @a@. This is due to a limitation of Template Haskell—without
-- the kind signature, 'makeRep1FromType' has no way of figuring out the kind of
-- @a@, and the generated type might be completely wrong as a result!
--
-- The use of 'makeRep1FromType' is generally discouraged, since 'makeRep1Inline'
-- does exactly the same thing but without having to go through an intermediate
-- type synonym, and as a result, 'makeRep1Inline' tends to be less buggy.
makeRep1FromType :: Name -> Q Type -> Q Type
makeRep1FromType n = makeRepCommon Generic1 TypeSynonymRep n . Just

makeRepCommon :: GenericClass
              -> RepOptions
              -> Name
              -> Maybe (Q Type)
              -> Q Type
makeRepCommon gClass repOpts n mbQTy = do
  i <- reifyDataInfo n
  let (name, isNT, declTvbs, cons, dv) = either error id i
  -- See Note [Forcing buildTypeInstance]
  !_ <- buildTypeInstance gClass False name declTvbs dv

  case (mbQTy, repOpts) of
       (Just qTy, TypeSynonymRep) -> qTy >>= makeRepTySynApp gClass dv name cons
       (Just qTy, InlineRep)      -> qTy >>= makeRepInline   gClass dv name isNT cons
       (Nothing,  TypeSynonymRep) -> conT $ genRepName gClass dv name
       (Nothing,  InlineRep)      -> fail "makeRepCommon"

makeRepInline :: GenericClass
              -> DataVariety
              -> Name
              -> Bool
              -> [Con]
              -> Type
              -> Q Type
makeRepInline gClass dv name isNT cons ty = do
  let instVars = map tyVarBndrToType $ requiredTyVarsOfType ty
  repType gClass dv name isNT cons instVars

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
  let instTvbs = map unKindedTV $ requiredTyVarsOfType ty
  -- We grab the type variables from the first constructor's type signature.
  -- Or, if there are no constructors, we grab no type variables. The latter
  -- is okay because we use zipWith to ensure that we never pass more type
  -- variables than the generated type synonym can accept.
  -- See Note [Arguments to generated type synonyms]
  tySynVars <- grabTyVarsFromCons gClass cons
  return . applyTyToTvbs (genRepName gClass dv name)
         $ zipWith const instTvbs tySynVars

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
  buildTypeInstance gClass False name allTvbs dv
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
        -> [Type]
        -> Q Type
repType gClass dv dt isNT cs tySynVars =
    conT d1TypeName `appT` mkMetaDataType dv dt isNT `appT`
      foldr1' sum' (conT v1TypeName)
        (map (repCon gClass dv dt tySynVars) cs)
  where
    sum' :: Q Type -> Q Type -> Q Type
    sum' a b = conT sumTypeName `appT` a `appT` b

repCon :: GenericClass
       -> DataVariety
       -> Name
       -> [Type]
       -> Con
       -> Q Type
repCon gClass dv dt tySynVars (NormalC n bts) = do
    let bangs = map fst bts
    ssis <- reifySelStrictInfo n bangs
    repConWith gClass dv dt n tySynVars Nothing ssis False False
repCon gClass dv dt tySynVars (RecC n vbts) = do
    let (selNames, bangs, _) = unzip3 vbts
    ssis <- reifySelStrictInfo n bangs
    repConWith gClass dv dt n tySynVars (Just selNames) ssis True False
repCon gClass dv dt tySynVars (InfixC t1 n t2) = do
    let bangs = map fst [t1, t2]
    ssis <- reifySelStrictInfo n bangs
    repConWith gClass dv dt n tySynVars Nothing ssis False True
repCon _ _ _ _ con = gadtError con

repConWith :: GenericClass
           -> DataVariety
           -> Name
           -> Name
           -> [Type]
           -> Maybe [Name]
           -> [SelStrictInfo]
           -> Bool
           -> Bool
           -> Q Type
repConWith gClass dv dt n tySynVars mbSelNames ssis isRecord isInfix = do
    (conVars, ts, gk) <- reifyConTys gClass n

    let structureType :: Q Type
        structureType = case ssis of
                             [] -> conT u1TypeName
                             _  -> foldr1 prodT f

        -- See Note [Substituting types in a constructor type signature]
        typeSubst :: TypeSubst
        typeSubst = Map.fromList $
          zip (nub $ concatMap             tyVarNamesOfType  conVars)
              (nub $ concatMap (map VarT . tyVarNamesOfType) tySynVars)

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
              Gen1 _ (Just _kvName) ->
-- See Note [Generic1 is polykinded on GHC 8.2]
#if __GLASGOW_HASKELL__ >= 801
                t
#else
                substNameWithKind _kvName starK t
#endif
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

-- | Deduces the instance type (and kind) to use for a Generic(1) instance.
buildTypeInstance :: GenericClass
                  -- ^ Generic or Generic1
                  -> KindSigOptions
                  -- ^ Whether or not to use explicit kind signatures in the instance type
                  -> Name
                  -- ^ The type constructor or data family name
                  -> [TyVarBndr]
                  -- ^ The type variables from the data type/data family declaration
                  -> DataVariety
                  -- ^ If using a data family instance, provides the types used
                  -- to instantiate the instance
                  -> Q (Type, Kind)
-- Plain data type/newtype case
buildTypeInstance gClass useKindSigs tyConName tvbs DataPlain =
    let varTys :: [Type]
        varTys = map tyVarBndrToType tvbs
    in buildTypeInstanceFromTys gClass useKindSigs tyConName varTys
-- Data family instance case
--
-- The CPP is present to work around a couple of annoying old GHC bugs.
-- See Note [Polykinded data families in Template Haskell]
buildTypeInstance gClass useKindSigs parentName tvbs (DataFamily _ instTysAndKinds) = do
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
        -- Because these type variables were eta-reduced away, we can only
        -- determine their kind by using stealKindForType. Therefore, we mark
        -- them as VarT to ensure they will be given an explicit kind annotation
        -- (and so the kind inference machinery has the right information).
        xTys = map VarT xTypeNames

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
    buildTypeInstanceFromTys gClass useKindSigs parentName instTys

-- For the given Types, deduces the instance type (and kind) to use for a
-- Generic(1) instance. Coming up with the instance type isn't as simple as
-- dropping the last types, as you need to be wary of kinds being instantiated
-- with *.
-- See Note [Type inference in derived instances]
buildTypeInstanceFromTys :: GenericClass
                         -- ^ Generic or Generic1
                         -> KindSigOptions
                         -- ^ Whether or not to use explicit kind signatures in the instance type
                         -> Name
                         -- ^ The type constructor or data family name
                         -> [Type]
                         -- ^ The types to instantiate the instance with
                         -> Q (Type, Kind)
buildTypeInstanceFromTys gClass useKindSigs tyConName varTysOrig = do
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

        -- Substitute kind * for any dropped kind variables
    let varTysExpSubst :: [Type]
-- See Note [Generic1 is polykinded on GHC 8.2]
#if __GLASGOW_HASKELL__ >= 801
        varTysExpSubst = varTysExp
#else
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

        droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati
#endif

    let remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

-- See Note [Generic1 is polykinded on GHC 8.2]
#if __GLASGOW_HASKELL__ < 801
    -- If any of the dropped types were polykinded, ensure that there are of
    -- kind * after substituting * for the dropped kind variables. If not,
    -- throw an error.
    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError tyConName
#endif

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
    let varTysOrigSubst :: [Type]
        varTysOrigSubst =
-- See Note [Generic1 is polykinded on GHC 8.2]
#if __GLASGOW_HASKELL__ >= 801
          id
#else
          map (substNamesWithKindStar droppedKindVarNames)
#endif
            $ varTysOrig

        remainingTysOrigSubst, droppedTysOrigSubst :: [Type]
        (remainingTysOrigSubst, droppedTysOrigSubst) =
            splitAt remainingLength varTysOrigSubst

        remainingTysOrigSubst' :: [Type]
        -- See Note [Kind signatures in derived instances] for an explanation
        -- of the useKindSigs check.
        remainingTysOrigSubst' =
          if useKindSigs
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceType :: Type
        instanceType = applyTyToTys (ConT tyConName) remainingTysOrigSubst'

        -- See Note [Kind signatures in derived instances]
        instanceKind :: Kind
        instanceKind = makeFunKind (map typeKind droppedTysOrigSubst) starK

    -- Ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceType, instanceKind)

-- See Note [Arguments to generated type synonyms]
grabTyVarsFromCons :: GenericClass -> [Con] -> Q [Type]
grabTyVarsFromCons _      []      = return []
grabTyVarsFromCons gClass (con:_) =
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
-XTypeInType enabled on GHC 8.0, you might have something like this:

  data Proxy (a :: k) (b :: k) = Proxy k deriving Generic1

Then k gets specialized to *, which means that k should NOT show up in the RHS of
a Rep1 type instance! To avoid this, make sure to substitute k with *.
See also Note [Generic1 is polykinded on GHC 8.2].

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

We generally include explicit type signatures in derived instances. One reason for
doing so is that in the case of certain data family instances, not including kind
signatures can result in ambiguity. For example, consider the following two data
family instances that are distinguished by their kinds:

  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)

If we dropped the kind signature for a in a derived instance for Fam a, then GHC
would have no way of knowing which instance we are talking about.

Another motivation for explicit kind signatures is the -XTypeInType extension.
With -XTypeInType, dropping kind signatures can completely change the meaning
of some data types. For example, there is a substantial difference between these
two data types:

  data T k (a :: k) = T k
  data T k a        = T k

In addition to using explicit kind signatures on type variables, we also put
explicit return kinds in the instance head, so generated instances will look
something like this:

  data S (a :: k) = S k
  instance Generic1 (S :: k -> *) where
    type Rep1 (S :: k -> *) = ... (Rec0 k)

Why do we do this? Imagine what the instance would be without the explicit return kind:

  instance Generic1 S where
    type Rep1 S = ... (Rec0 k)

This is an error, since the variable k is now out-of-scope!

Although explicit kind signatures are the right thing to do in most cases, there
are sadly some degenerate cases where this isn't true. Consider this example:

  newtype Compose (f :: k2 -> *) (g :: k1 -> k2) (a :: k1) = Compose (f (g a))

The Rep1 type instance in a Generic1 instance for Compose would involve the type
(f :.: Rec1 g), which forces (f :: * -> *). But this library doesn't have very
sophisticated kind inference machinery (other than what is mentioned in
Note [Substituting types in constructor type signatures]), so at the moment we
have no way of actually unifying k1 with *. So the naïve generated Generic1
instance would be:

  instance Generic1 (Compose (f :: k2 -> *) (g :: k1 -> k2)) where
    type Rep1 (Compose f g) = ... (f :.: Rec1 g)

This is wrong, since f's kind is overly generalized. To get around this issue,
there are variants of the TH functions that allow you to configure the KindSigOptions.
If KindSigOptions is set to False, then generated instances will not include
explicit kind signatures, leaving it up to GHC's kind inference machinery to
figure out the correct kinds.

Note [Generic1 is polykinded on GHC 8.2]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Prior to GHC 8.2, Generic1 :: (* -> *) -> Constraint. This means that if a Generic1
instance is defined for a polykinded data type like so:

  data Proxy k (a :: k) = Proxy

Then k is unified with *, and this has an effect on the generated Generic1 instance:

  instance Generic1 (Proxy *) where ...

We must take great care to ensure that all occurrences of k are substituted with *,
or else the generated instance will be ill kinded.

On GHC 8.2 and later, Generic1 :: (k -> *) -> Constraint. This means we don't have
to do this kind unification anymore! Hooray!
-}
