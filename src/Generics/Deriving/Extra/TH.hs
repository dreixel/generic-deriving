{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

{- |
Module      :  Generics.Deriving.Extra.TH
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
module Generics.Deriving.Extra.TH (
      deriveAll
    , deriveAll0
    , deriveAll1
    , deriveAll0And1
    , deriveRepresentable0
    , deriveRepresentable1
    , deriveRep0
    , deriveRep1

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
    , EmptyCaseOptions
    , defaultEmptyCaseOptions

    -- ** Functions with optional arguments
    , deriveAll0Options
    , deriveAll1Options
    , deriveAll0And1Options
    , deriveRepresentable0Options
    , deriveRepresentable1Options
    , deriveRep0Options
    , deriveRep1Options

    , makeFrom0Options
    , makeTo0Options
    , makeFrom1Options
    , makeTo1Options
  ) where

import           Control.Monad ((>=>), unless, when)

import qualified Data.Map as Map (empty, fromList)
import           Data.Maybe (fromMaybe)

import           Generics.Deriving.Extra.TH.Internal

import           Language.Haskell.TH.Datatype as D
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

* 'EmptyCaseOptions': By default, all derived instances for empty data types
  (i.e., data types with no constructors) use 'error' in @from(1)@/@to(1)@.
  For instance, @data Empty@ would have this derived 'Generic' instance:

  @
  instance Generic Empty where
    type Rep Empty = D1 ('MetaData ...) V1
    from _ = M1 (error "No generic representation for empty datatype Empty")
    to (M1 _) = error "No generic representation for empty datatype Empty"
  @

  This matches the behavior of GHC up until 8.4, when derived @Generic(1)@
  instances began to use the @EmptyCase@ extension. In GHC 8.4, the derived
  'Generic' instance for @Empty@ would instead be:

  @
  instance Generic Empty where
    type Rep Empty = D1 ('MetaData ...) V1
    from x = M1 (case x of {})
    to (M1 x) = case x of {}
  @

  This is a slightly better encoding since, for example, any divergent
  computations passed to 'from' will actually diverge (as opposed to before,
  where the result would always be a call to 'error'). On the other hand, using
  this encoding in @generic-deriving@ has one large drawback: it requires
  enabling @EmptyCase@, an extension which was only introduced in GHC 7.8
  (and only received reliable pattern-match coverage checking in 8.2).

  The 'EmptyCaseOptions' field controls whether code should be emitted that
  uses @EmptyCase@ (i.e., 'EmptyCaseOptions' set to 'True') or not ('False').
  The default value is 'False'. Note that even if set to 'True', this option
  has no effect on GHCs before 7.8, as @EmptyCase@ did not exist then.
-}

-- | Additional options for configuring derived 'Generic'/'Generic1' instances
-- using Template Haskell.
data Options = Options
  { repOptions       :: RepOptions
  , kindSigOptions   :: KindSigOptions
  , emptyCaseOptions :: EmptyCaseOptions
  } deriving (Eq, Ord, Read, Show)

-- | Sensible default 'Options'.
defaultOptions :: Options
defaultOptions = Options
  { repOptions       = defaultRepOptions
  , kindSigOptions   = defaultKindSigOptions
  , emptyCaseOptions = defaultEmptyCaseOptions
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

-- | 'True' if generated code for empty data types should use the @EmptyCase@
-- extension, 'False' otherwise. This has no effect on GHCs before 7.8, since
-- @EmptyCase@ is only available in 7.8 or later.
type EmptyCaseOptions = Bool

-- | Sensible default 'EmptyCaseOptions'.
defaultEmptyCaseOptions :: EmptyCaseOptions
defaultEmptyCaseOptions = False

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
    a <- if generic
            then deriveRepresentableCommon Generic opts n
            else return []
    b <- if generic1
            then deriveRepresentableCommon Generic1 opts n
            else return []
    return (a ++ b)

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
  let (name, instTys, cons, dv) = either error id i
  -- See Note [Forcing buildTypeInstance]
  !_ <- buildTypeInstance gClass useKindSigs name instTys

  -- See Note [Kind signatures in derived instances]
  let (tySynVars, gk) = genericKind gClass instTys
      tySynVars' = if useKindSigs
                      then tySynVars
                      else map unKindedTV tySynVars
  fmap (:[]) $ tySynD (genRepName gClass dv name)
                      tySynVars'
                      (repType gk dv name Map.empty cons)

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
  let (name, instTys, cons, dv) = either error id i
      useKindSigs = kindSigOptions opts
  -- See Note [Forcing buildTypeInstance]
  !(origTy, origKind) <- buildTypeInstance gClass useKindSigs name instTys
  tyInsRHS <- if repOptions opts == InlineRep
                 then makeRepInline   gClass dv name instTys cons origTy
                 else makeRepTySynApp gClass dv name              origTy

  let origSigTy = if useKindSigs
                     then SigT origTy origKind
                     else origTy
  tyIns <- tySynInstDCompat repName Nothing [return origSigTy] (return tyInsRHS)
  let ecOptions = emptyCaseOptions opts
      mkBody maker = [clause [] (normalB $
        mkCaseExp gClass ecOptions name instTys cons maker) []]
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
  let (name, instTys, cons, dv) = either error id i
  -- See Note [Forcing buildTypeInstance]
  !_ <- buildTypeInstance gClass False name instTys

  case (mbQTy, repOpts) of
       (Just qTy, TypeSynonymRep) -> qTy >>= makeRepTySynApp gClass dv name
       (Just qTy, InlineRep)      -> qTy >>= makeRepInline   gClass dv name instTys cons
       (Nothing,  TypeSynonymRep) -> conT $ genRepName gClass dv name
       (Nothing,  InlineRep)      -> fail "makeRepCommon"

makeRepInline :: GenericClass
              -> DatatypeVariant_
              -> Name
              -> [Type]
              -> [ConstructorInfo]
              -> Type
              -> Q Type
makeRepInline gClass dv name instTys cons ty = do
  let instVars = freeVariablesWellScoped [ty]
      (tySynVars, gk)  = genericKind gClass instTys

      typeSubst :: TypeSubst
      typeSubst = Map.fromList $
        zip (map tvName tySynVars)
            (map (VarT . tvName) instVars)

  repType gk dv name typeSubst cons

makeRepTySynApp :: GenericClass -> DatatypeVariant_ -> Name
                -> Type -> Q Type
makeRepTySynApp gClass dv name ty =
  -- Here, we figure out the distinct type variables (in order from left-to-right)
  -- of the LHS of the Rep(1) instance. We call unKindedTV because the kind
  -- inferencer can figure out the kinds perfectly well, so we don't need to
  -- give anything here explicit kind signatures.
  let instTvbs = map unKindedTV $ freeVariablesWellScoped [ty]
  in return $ applyTyToTvbs (genRepName gClass dv name) instTvbs

-- | A backwards-compatible synonym for 'makeFrom0'.
makeFrom :: Name -> Q Exp
makeFrom = makeFrom0

-- | Generates a lambda expression which behaves like 'from'.
makeFrom0 :: Name -> Q Exp
makeFrom0 = makeFrom0Options defaultEmptyCaseOptions

-- | Like 'makeFrom0Options', but takes an 'EmptyCaseOptions' argument.
makeFrom0Options :: EmptyCaseOptions -> Name -> Q Exp
makeFrom0Options = makeFunCommon mkFrom Generic

-- | A backwards-compatible synonym for 'makeTo0'.
makeTo :: Name -> Q Exp
makeTo = makeTo0

-- | Generates a lambda expression which behaves like 'to'.
makeTo0 :: Name -> Q Exp
makeTo0 = makeTo0Options defaultEmptyCaseOptions

-- | Like 'makeTo0Options', but takes an 'EmptyCaseOptions' argument.
makeTo0Options :: EmptyCaseOptions -> Name -> Q Exp
makeTo0Options = makeFunCommon mkTo Generic

-- | Generates a lambda expression which behaves like 'from1'.
makeFrom1 :: Name -> Q Exp
makeFrom1 = makeFrom1Options defaultEmptyCaseOptions

-- | Like 'makeFrom1Options', but takes an 'EmptyCaseOptions' argument.
makeFrom1Options :: EmptyCaseOptions -> Name -> Q Exp
makeFrom1Options = makeFunCommon mkFrom Generic1

-- | Generates a lambda expression which behaves like 'to1'.
makeTo1 :: Name -> Q Exp
makeTo1 = makeTo1Options defaultEmptyCaseOptions

-- | Like 'makeTo1Options', but takes an 'EmptyCaseOptions' argument.
makeTo1Options :: EmptyCaseOptions -> Name -> Q Exp
makeTo1Options = makeFunCommon mkTo Generic1

makeFunCommon
  :: (GenericClass -> EmptyCaseOptions ->  Int -> Int -> Name -> [Type]
                   -> [ConstructorInfo] -> Q Match)
  -> GenericClass -> EmptyCaseOptions -> Name -> Q Exp
makeFunCommon maker gClass ecOptions n = do
  i <- reifyDataInfo n
  let (name, instTys, cons, _) = either error id i
  -- See Note [Forcing buildTypeInstance]
  buildTypeInstance gClass False name instTys
    `seq` mkCaseExp gClass ecOptions name instTys cons maker

genRepName :: GenericClass -> DatatypeVariant_
           -> Name -> Name
genRepName gClass dv n
  = mkName
  . showsDatatypeVariant dv
  . (("Rep" ++ show (fromEnum gClass)) ++)
  . ((showNameQual n ++ "_") ++)
  . sanitizeName
  $ nameBase n

repType :: GenericKind
        -> DatatypeVariant_
        -> Name
        -> TypeSubst
        -> [ConstructorInfo]
        -> Q Type
repType gk dv dt typeSubst cs =
    conT d1TypeName `appT` mkMetaDataType dv dt `appT`
      foldBal sum' (conT v1TypeName) (map (repCon gk dv dt typeSubst) cs)
  where
    sum' :: Q Type -> Q Type -> Q Type
    sum' a b = conT sumTypeName `appT` a `appT` b

repCon :: GenericKind
       -> DatatypeVariant_
       -> Name
       -> TypeSubst
       -> ConstructorInfo
       -> Q Type
repCon gk dv dt typeSubst
  (ConstructorInfo { constructorName       = n
                   , constructorVars       = vars
                   , constructorContext    = ctxt
                   , constructorStrictness = bangs
                   , constructorFields     = ts
                   , constructorVariant    = cv
                   }) = do
  checkExistentialVars n vars
  let mbSelNames = case cv of
                     NormalConstructor          -> Nothing
                     InfixConstructor           -> Nothing
                     RecordConstructor selNames -> Just selNames
      isRecord   = case cv of
                     NormalConstructor   -> False
                     InfixConstructor    -> False
                     RecordConstructor _ -> True
      isInfix    = case cv of
                     NormalConstructor   -> False
                     InfixConstructor    -> True
                     RecordConstructor _ -> False
  ssis <- reifySelStrictInfo n bangs
  repConWith gk dv dt n typeSubst ctxt mbSelNames ssis ts isRecord isInfix

repConWith :: GenericKind
           -> DatatypeVariant_
           -> Name
           -> Name
           -> TypeSubst
           -> Cxt
           -> Maybe [Name]
           -> [SelStrictInfo]
           -> [Type]
           -> Bool
           -> Bool
           -> Q Type
repConWith gk dv dt n typeSubst ctxt mbSelNames ssis ts isRecord isInfix = do
    let structureType :: Q Type
        structureType = foldBal prodT (conT u1TypeName) f

        mkExContext :: Q Type -> Q Type
        mkExContext t
          | null ctxt = t
          | otherwise = do
              ctxt' <- repCxt gk typeSubst ctxt
              conT exContextTypeName
                `appT` return (cxtToConstraintTuple ctxt')
                `appT` t

        cxtToConstraintTuple :: Cxt -> Type
        cxtToConstraintTuple [p] = p
        cxtToConstraintTuple c   = applyTyToTys (TupleT (length c)) c

        f :: [Q Type]
        f = case mbSelNames of
                 Just selNames -> zipWith3 (repField gk dv dt n typeSubst . Just)
                                           selNames ssis ts
                 Nothing       -> zipWith  (repField gk dv dt n typeSubst Nothing)
                                           ssis ts

    conT c1TypeName
      `appT` mkMetaConsType dv dt n isRecord isInfix
      `appT` mkExContext structureType

prodT :: Q Type -> Q Type -> Q Type
prodT a b = conT productTypeName `appT` a `appT` b

repCxt :: GenericKind -> TypeSubst -> Cxt -> Q Cxt
repCxt gk typeSubst ctxt = do
  ctxt' <- mapM resolveTypeSynonyms (applySubstitution typeSubst ctxt)
  mapM (repPred gk) ctxt'

repPred :: GenericKind -> Pred -> Q Pred
repPred gk pred' = do
  case gk of
    Gen0 -> return ()
    Gen1 name _ -> do
      when (not $ pred' `ground` name)
        tyVarInExContextError

  -- EqualityT can refer to both homogeneous and heterogeneous equality, but TH
  -- always splices EqualityT back in as if it were homogeneous. To be on the
  -- safe side, always conservatively assume that the equality is
  -- heterogeneous, since it is more permissive.
  return $ case asEqualPred pred' of
    Just (ty1, ty2) -> ConT heqTypeName `AppT` ty1 `AppT` ty2
    Nothing         -> pred'

repField :: GenericKind
         -> DatatypeVariant_
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
    `appT` (repFieldArg gk =<< resolveTypeSynonyms (applySubstitution typeSubst t))

repFieldArg :: GenericKind -> Type -> Q Type
repFieldArg _ ForallT{} = rankNError
repFieldArg gk (SigT t _) = repFieldArg gk t
repFieldArg Gen0 t = boxT t
repFieldArg (Gen1 name _) (VarT t) | t == name = conT par1TypeName
repFieldArg gk@(Gen1 name _) t = do
  let (tyHead, tyArgs)   = unapplyTy t
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

  itf <- isInTypeFamilyApp name tyHead tyArgs
  if any (not . (`ground` name)) lhsArgs || itf
     then outOfPlaceTyVarError
     else case rhsArgs of
          []   -> rec0Type
          ty:_ -> inspectTy ty

boxT :: Type -> Q Type
boxT ty = case unboxedRepNames ty of
    Just (boxTyName, _, _) -> conT boxTyName
    Nothing                -> conT rec0TypeName `appT` return ty

mkCaseExp
  :: GenericClass -> EmptyCaseOptions -> Name -> [Type] -> [ConstructorInfo]
  -> (GenericClass -> EmptyCaseOptions -> Int -> Int -> Name -> [Type]
                   -> [ConstructorInfo] -> Q Match)
  -> Q Exp
mkCaseExp gClass ecOptions dt instTys cs matchmaker = do
  val <- newName "val"
  lam1E (varP val) $ caseE (varE val) [matchmaker gClass ecOptions 1 1 dt instTys cs]

mkFrom :: GenericClass -> EmptyCaseOptions -> Int -> Int -> Name -> [Type]
       -> [ConstructorInfo] -> Q Match
mkFrom gClass ecOptions m i dt instTys cs = do
    y <- newName "y"
    match (varP y)
          (normalB $ conE m1DataName `appE` caseE (varE y) cases)
          []
  where
    cases = case cs of
              [] -> errorFrom ecOptions dt
              _  -> zipWith (fromCon gk wrapE (length cs)) [1..] cs
    wrapE e = lrE i m e
    (_, gk) = genericKind gClass instTys

errorFrom :: EmptyCaseOptions -> Name -> [Q Match]
errorFrom useEmptyCase dt
  | useEmptyCase
  = []
  | otherwise
  = [do z <- newName "z"
        match
          (varP z)
          (normalB $
            appE (varE seqValName) (varE z) `appE`
            appE (varE errorValName)
                 (stringE $ "No generic representation for empty datatype "
                          ++ nameBase dt))
          []]

mkTo :: GenericClass -> EmptyCaseOptions -> Int -> Int -> Name -> [Type]
     -> [ConstructorInfo] -> Q Match
mkTo gClass ecOptions m i dt instTys cs = do
    y <- newName "y"
    match (conP m1DataName [varP y])
          (normalB $ caseE (varE y) cases)
          []
  where
    cases = case cs of
              [] -> errorTo ecOptions dt
              _  -> zipWith (toCon gk wrapP (length cs)) [1..] cs
    wrapP p = lrP i m p
    (_, gk) = genericKind gClass instTys

errorTo :: EmptyCaseOptions -> Name -> [Q Match]
errorTo useEmptyCase dt
  | useEmptyCase
  = []
  | otherwise
  = [do z <- newName "z"
        match
          (varP z)
          (normalB $
            appE (varE seqValName) (varE z) `appE`
            appE (varE errorValName)
                 (stringE $ "No values for empty datatype " ++ nameBase dt))
          []]

fromCon :: GenericKind -> (Q Exp -> Q Exp) -> Int -> Int
        -> ConstructorInfo -> Q Match
fromCon gk wrap m i
  (ConstructorInfo { constructorName    = cn
                   , constructorVars    = vars
                   , constructorContext = ctxt
                   , constructorFields  = ts
                   }) = do
  checkExistentialVars cn vars
  fNames <- newNameList "f" $ length ts
  match (conP cn (map varP fNames))
        (normalB $ wrap $ lrE i m $ appE (conE m1DataName) $ mkExContext $
          foldBal prodE (conE u1DataName) (zipWith (fromField gk) fNames ts)) []
  where
    mkExContext :: Q Exp -> Q Exp
    mkExContext = case ctxt of
      [] -> id
      _  -> appE (conE suchThatDataName)

prodE :: Q Exp -> Q Exp -> Q Exp
prodE x y = conE productDataName `appE` x `appE` y

fromField :: GenericKind -> Name -> Type -> Q Exp
fromField gk nr t = conE m1DataName `appE` (fromFieldWrap gk nr =<< resolveTypeSynonyms t)

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
      let (tyHead, tyArgs)   = unapplyTy t
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

      itf <- isInTypeFamilyApp name tyHead tyArgs
      if any (not . (`ground` name)) lhsArgs || itf
         then outOfPlaceTyVarError
         else case rhsArgs of
              []   -> conE $ boxRepName t
              ty:_ -> inspectTy ty

boxRepName :: Type -> Name
boxRepName = maybe k1DataName snd3 . unboxedRepNames

toCon :: GenericKind -> (Q Pat -> Q Pat) -> Int -> Int
      -> ConstructorInfo -> Q Match
toCon gk wrap m i
  (ConstructorInfo { constructorName    = cn
                   , constructorVars    = vars
                   , constructorContext = ctxt
                   , constructorFields  = ts
                   }) = do
  checkExistentialVars cn vars
  fNames <- newNameList "f" $ length ts
  match (wrap $ lrP i m $ conP m1DataName $ mkExContext
          [foldBal prod (conP u1DataName []) (zipWith (toField gk) fNames ts)])
        (normalB $ foldl appE (conE cn)
                         (zipWith (\nr -> resolveTypeSynonyms >=> toConUnwC gk nr)
                         fNames ts)) []
  where
    mkExContext :: [Q Pat] -> [Q Pat]
    mkExContext ps =
      case ctxt of
        [] -> ps
        _  -> [conP suchThatDataName ps]

    prod :: Q Pat -> Q Pat -> Q Pat
    prod x y = conP productDataName [x,y]

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
      let (tyHead, tyArgs)   = unapplyTy t
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

      itf <- isInTypeFamilyApp name tyHead tyArgs
      if any (not . (`ground` name)) lhsArgs || itf
         then outOfPlaceTyVarError
         else case rhsArgs of
              []   -> varE $ unboxRepName t
              ty:_ -> inspectTy ty

unboxRepName :: Type -> Name
unboxRepName = maybe unK1ValName trd3 . unboxedRepNames

lrP :: Int -> Int -> (Q Pat -> Q Pat)
lrP i n p
  | n == 0       = fail "lrP: impossible"
  | n == 1       = p
  | i <= div n 2 = conP l1DataName [lrP i     (div n 2) p]
  | otherwise    = conP r1DataName [lrP (i-m) (n-m)     p]
                     where m = div n 2

lrE :: Int -> Int -> (Q Exp -> Q Exp)
lrE i n e
  | n == 0       = fail "lrE: impossible"
  | n == 1       = e
  | i <= div n 2 = conE l1DataName `appE` lrE i     (div n 2) e
  | otherwise    = conE r1DataName `appE` lrE (i-m) (n-m)     e
                     where m = div n 2

unboxedRepNames :: Type -> Maybe (Name, Name, Name)
unboxedRepNames ty
  | ty == ConT addrHashTypeName   = Just (uAddrTypeName,   uAddrDataName,   uAddrHashValName)
  | ty == ConT charHashTypeName   = Just (uCharTypeName,   uCharDataName,   uCharHashValName)
  | ty == ConT doubleHashTypeName = Just (uDoubleTypeName, uDoubleDataName, uDoubleHashValName)
  | ty == ConT floatHashTypeName  = Just (uFloatTypeName,  uFloatDataName,  uFloatHashValName)
  | ty == ConT intHashTypeName    = Just (uIntTypeName,    uIntDataName,    uIntHashValName)
  | ty == ConT wordHashTypeName   = Just (uWordTypeName,   uWordDataName,   uWordHashValName)
  | otherwise                     = Nothing

-- For the given Types, deduces the instance type (and kind) to use for a
-- Generic(1) instance. Coming up with the instance type isn't as simple as
-- dropping the last types, as you need to be wary of kinds being instantiated
-- with *.
-- See Note [Type inference in derived instances]
buildTypeInstance :: GenericClass
                  -- ^ Generic or Generic1
                  -> KindSigOptions
                  -- ^ Whether or not to use explicit kind signatures in the instance type
                  -> Name
                  -- ^ The type constructor or data family name
                  -> [Type]
                  -- ^ The types to instantiate the instance with
                  -> Q (Type, Kind)
buildTypeInstance gClass useKindSigs tyConName varTysOrig = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM resolveTypeSynonyms varTysOrig

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
        -- See Note [Generic1 is polykinded in base-4.10]
        varTysExpSubst = varTysExp

    let remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

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
        -- See Note [Generic1 is polykinded in base-4.10]
        varTysOrigSubst = varTysOrig

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

mkMetaDataType :: DatatypeVariant_ -> Name -> Q Type
mkMetaDataType dv n =
           promotedT metaDataDataName
    `appT` litT (strTyLit (nameBase n))
    `appT` litT (strTyLit m)
    `appT` litT (strTyLit pkg)
    `appT` promoteBool (isNewtypeVariant dv)
  where
    m, pkg :: String
    m   = fromMaybe (error "Cannot fetch module name!")  (nameModule n)
    pkg = fromMaybe (error "Cannot fetch package name!") (namePackage n)

mkMetaConsType :: DatatypeVariant_ -> Name -> Name -> Bool -> Bool -> Q Type
mkMetaConsType _ _ n conIsRecord conIsInfix = do
    mbFi <- reifyFixity n
    promotedT metaConsDataName
      `appT` litT (strTyLit (nameBase n))
      `appT` fixityIPromotedType mbFi conIsInfix
      `appT` promoteBool conIsRecord

promoteBool :: Bool -> Q Type
promoteBool True  = promotedT trueDataName
promoteBool False = promotedT falseDataName

fixityIPromotedType :: Maybe Fixity -> Bool -> Q Type
fixityIPromotedType mbFi True =
           promotedT infixIDataName
    `appT` promoteAssociativity a
    `appT` litT (numTyLit (toInteger n))
  where
    Fixity n a = fromMaybe defaultFixity mbFi
fixityIPromotedType _ False = promotedT prefixIDataName

promoteAssociativity :: FixityDirection -> Q Type
promoteAssociativity InfixL = promotedT leftAssociativeDataName
promoteAssociativity InfixR = promotedT rightAssociativeDataName
promoteAssociativity InfixN = promotedT notAssociativeDataName

mkMetaSelType :: DatatypeVariant_ -> Name -> Name -> Maybe Name
              -> SelStrictInfo -> Q Type
mkMetaSelType _ _ _ mbF (SelStrictInfo su ss ds) =
    let mbSelNameT = case mbF of
            Just f  -> promotedT justDataName `appT` litT (strTyLit (nameBase f))
            Nothing -> promotedT nothingDataName
    in promotedT metaSelDataName
        `appT` mbSelNameT
        `appT` promoteUnpackedness su
        `appT` promoteStrictness ss
        `appT` promoteDecidedStrictness ds

data SelStrictInfo = SelStrictInfo Unpackedness Strictness DecidedStrictness

promoteUnpackedness :: Unpackedness -> Q Type
promoteUnpackedness UnspecifiedUnpackedness = promotedT noSourceUnpackednessDataName
promoteUnpackedness NoUnpack                = promotedT sourceNoUnpackDataName
promoteUnpackedness Unpack                  = promotedT sourceUnpackDataName

promoteStrictness :: Strictness -> Q Type
promoteStrictness UnspecifiedStrictness = promotedT noSourceStrictnessDataName
promoteStrictness Lazy                  = promotedT sourceLazyDataName
promoteStrictness D.Strict              = promotedT sourceStrictDataName

promoteDecidedStrictness :: DecidedStrictness -> Q Type
promoteDecidedStrictness DecidedLazy   = promotedT decidedLazyDataName
promoteDecidedStrictness DecidedStrict = promotedT decidedStrictDataName
promoteDecidedStrictness DecidedUnpack = promotedT decidedUnpackDataName

reifySelStrictInfo :: Name -> [FieldStrictness] -> Q [SelStrictInfo]
reifySelStrictInfo conName fs = do
    dcdStrs <- reifyConStrictness conName
    let srcUnpks = map fieldUnpackedness fs
        srcStrs  = map fieldStrictness   fs
    return $ zipWith3 SelStrictInfo srcUnpks srcStrs dcdStrs

{-
Note [Forcing buildTypeInstance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sometimes, we don't explicitly need to generate a Generic(1) type instance, but
we force buildTypeInstance nevertheless. This is because it performs some checks
for whether or not the provided datatype can actually have Generic(1) implemented for
it, and produces errors if it can't. Otherwise, laziness would cause these checks
to be skipped entirely, which could result in some indecipherable type errors
down the road.

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
Note [Generic1 is polykinded in base-4.10]), so at the moment we
have no way of actually unifying k1 with *. So the naïve generated Generic1
instance would be:

  instance Generic1 (Compose (f :: k2 -> *) (g :: k1 -> k2)) where
    type Rep1 (Compose f g) = ... (f :.: Rec1 g)

This is wrong, since f's kind is overly generalized. To get around this issue,
there are variants of the TH functions that allow you to configure the KindSigOptions.
If KindSigOptions is set to False, then generated instances will not include
explicit kind signatures, leaving it up to GHC's kind inference machinery to
figure out the correct kinds.

Note [Generic1 is polykinded in base-4.10]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Prior to base-4.10, Generic1 :: (* -> *) -> Constraint. This means that if a Generic1
instance is defined for a polykinded data type like so:

  data Proxy k (a :: k) = Proxy

Then k is unified with *, and this has an effect on the generated Generic1 instance:

  instance Generic1 (Proxy *) where ...

We must take great care to ensure that all occurrences of k are substituted with *,
or else the generated instance will be ill kinded.

In base-4.10 and later, Generic1 :: (k -> *) -> Constraint. This means we don't have
to do any of this kind unification trickery anymore! Hooray!
-}
