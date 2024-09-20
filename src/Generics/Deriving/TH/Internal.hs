{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- |
Module      :  Generics.Deriving.TH.Internal
Copyright   :  (c) 2008--2009 Universiteit Utrecht
License     :  BSD3

Maintainer  :  generics@haskell.org
Stability   :  experimental
Portability :  non-portable

Template Haskell-related utilities.
-}

module Generics.Deriving.TH.Internal where

import           Control.Monad (unless)

import           Data.Char (isAlphaNum, ord)
import           Data.Foldable (foldr')
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Map as Map (Map)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Set (Set)

import qualified Generics.Deriving as GD
import           Generics.Deriving hiding
                   ( DecidedStrictness(..), Fixity(Infix)
                   , SourceStrictness(..), SourceUnpackedness(..)
                   , datatypeName
                   )

import           GHC.Exts (Addr#, Char#, Double#, Float#, Int#, Word#)

import           Language.Haskell.TH.Datatype as Datatype
import           Language.Haskell.TH.Datatype.TyVarBndr
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr (pprint)
import           Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

type TypeSubst = Map Name Type

applySubstitutionKind :: Map Name Kind -> Type -> Type
applySubstitutionKind = applySubstitution

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = applySubstitutionKind (Map.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (flip substNameWithKind starK) t ns

-------------------------------------------------------------------------------
-- StarKindStatus
-------------------------------------------------------------------------------

-- | Whether a type is of kind @*@, a kind variable, or some other kind. The
-- kind variable case is given special treatment solely to support GHC 8.0 and
-- earlier, in which Generic1 was not poly-kinded. In order to support deriving
-- Generic1 instances on these versions of GHC, we must substitute such kinds
-- with @*@ to ensure that the resulting instance is well kinded.
-- See @Note [Generic1 is polykinded in base-4.10]@ in "Generics.Deriving.TH".
data StarKindStatus = KindStar
                    | IsKindVar Name
                    | OtherKind
  deriving Eq

-- | Does a Type have kind * or k (for some kind variable k)?
canRealizeKindStar :: Type -> StarKindStatus
canRealizeKindStar t
  | hasKindStar t = KindStar
  | otherwise = case t of
                     SigT _ (VarT k) -> IsKindVar k
                     _               -> OtherKind

-- | Returns 'Just' the kind variable 'Name' of a 'StarKindStatus' if it exists.
-- Otherwise, returns 'Nothing'.
starKindStatusToName :: StarKindStatus -> Maybe Name
starKindStatusToName (IsKindVar n) = Just n
starKindStatusToName _             = Nothing

-- | Concat together all of the StarKindStatuses that are IsKindVar and extract
-- the kind variables' Names out.
catKindVarNames :: [StarKindStatus] -> [Name]
catKindVarNames = mapMaybe starKindStatusToName

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

-- | Returns True if a Type has kind *.
hasKindStar :: Type -> Bool
hasKindStar VarT{}         = True
hasKindStar (SigT _ StarT) = True
hasKindStar _              = False

-- | Converts a VarT or a SigT into Just the corresponding TyVarBndr.
-- Converts other Types to Nothing.
typeToTyVarBndr :: Type -> Maybe TyVarBndrUnit
typeToTyVarBndr (VarT n)          = Just (plainTV n)
typeToTyVarBndr (SigT (VarT n) k) = Just (kindedTV n k)
typeToTyVarBndr _                 = Nothing

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
typeKind :: Type -> Kind
typeKind (SigT _ k) = k
typeKind _          = starK

-- | Turns
--
-- @
-- [a, b] c
-- @
--
-- into
--
-- @
-- a -> b -> c
-- @
makeFunType :: [Type] -> Type -> Type
makeFunType argTys resTy = foldr' (AppT . AppT ArrowT) resTy argTys

-- | Turns
--
-- @
-- [k1, k2] k3
-- @
--
-- into
--
-- @
-- k1 -> k2 -> k3
-- @
makeFunKind :: [Kind] -> Kind -> Kind
makeFunKind = makeFunType

-- | Remove any outer `SigT` and `ParensT` constructors, and turn
-- an outermost `InfixT` constructor into plain applications.
dustOff :: Type -> Type
dustOff (SigT ty _) = dustOff ty
dustOff (ParensT ty) = dustOff ty
dustOff (InfixT ty1 n ty2) = ConT n `AppT` ty1 `AppT` ty2
dustOff ty = ty

-- | Checks whether a type is an unsaturated type family
-- application.
isUnsaturatedType :: Type -> Q Bool
isUnsaturatedType = go 0 . dustOff
  where
    -- Expects its argument to be dusted
    go :: Int -> Type -> Q Bool
    go d t = case t of
      ConT tcName -> check d tcName
      AppT f _ -> go (d + 1) (dustOff f)
      _ -> return False

    check :: Int -> Name -> Q Bool
    check d tcName = do
      mbinders <- getTypeFamilyBinders tcName
      return $ case mbinders of
        Just bndrs -> length bndrs > d
        Nothing -> False

-- | Given a name, check if that name is a type family. If
-- so, return a list of its binders.
getTypeFamilyBinders :: Name -> Q (Maybe [TyVarBndrVis])
getTypeFamilyBinders tcName = do
      info <- reify tcName
      return $ case info of
        FamilyI (OpenTypeFamilyD (TypeFamilyHead _ bndrs _ _)) _
          -> Just bndrs
        FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ bndrs _ _) _) _
          -> Just bndrs
        _ -> Nothing

-- | True if the type does not mention the Name
ground :: Type -> Name -> Bool
ground ty name = name `notElem` freeVariables ty

-- | Construct a type via curried application.
applyTyToTys :: Type -> [Type] -> Type
applyTyToTys = List.foldl' AppT

-- | Apply a type constructor name to type variable binders.
applyTyToTvbs :: Name -> [TyVarBndr_ flag] -> Type
applyTyToTvbs = List.foldl' (\a -> AppT a . tyVarBndrToType) . ConT

-- | Split a type signature by the arrows on its spine. For example, this:
--
-- @
-- forall a b. (a -> b) -> Char -> ()
-- @
--
-- would split to this:
--
-- @
-- ([a, b], [a -> b, Char, ()])
-- @
uncurryTy :: Type -> ([TyVarBndrSpec], [Type])
uncurryTy (AppT (AppT ArrowT t1) t2) =
  let (tvbs, tys) = uncurryTy t2
  in (tvbs, t1:tys)
uncurryTy (SigT t _) = uncurryTy t
uncurryTy (ForallT tvbs _ t) =
  let (tvbs', tys) = uncurryTy t
  in (tvbs ++ tvbs', tys)
uncurryTy t = ([], [t])

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> ([TyVarBndrSpec], [Kind])
uncurryKind = uncurryTy

tyVarBndrToType :: TyVarBndr_ flag -> Type
tyVarBndrToType = elimTV VarT (\n k -> SigT (VarT n) k)

-- | Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = mapM (newName . (prefix ++) . show) [1..n]

-- | Checks to see if the last types in a data family instance can be safely eta-
-- reduced (i.e., dropped), given the other types. This checks for three conditions:
--
-- (1) All of the dropped types are type variables
-- (2) All of the dropped types are distinct
-- (3) None of the remaining types mention any of the dropped types
canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
       -- Make sure not to pass something of type [Type], since Type
       -- didn't have an Ord instance until template-haskell-2.10.0.0
    && allDistinct droppedNames
    && not (any (`mentionsName` droppedNames) remaining)
  where
    droppedNames :: [Name]
    droppedNames = map varTToName dropped

-- | Extract the Name from a type variable. If the argument Type is not a
-- type variable, throw an error.
varTToName :: Type -> Name
varTToName (VarT n)   = n
varTToName (SigT t _) = varTToName t
varTToName _          = error "Not a type variable!"

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar VarT{}     = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Is the given kind a variable?
isKindVar :: Kind -> Bool
isKindVar = isTyVar

-- | Returns 'True' is a 'Type' contains no type variables.
isTypeMonomorphic :: Type -> Bool
isTypeMonomorphic = go
  where
    go :: Type -> Bool
    go (AppT t1 t2) = go t1 && go t2
    go (SigT t k)   = go t && go k
    go VarT{}       = False
    go _            = True

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- | Peel off a kind signature from a TyVarBndr (if it has one).
unKindedTV :: TyVarBndrUnit -> TyVarBndrUnit
unKindedTV tvb = elimTV (\_ -> tvb) (\n _ -> plainTV n) tvb

-- | Does the given type mention any of the Names in the list?
mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t k)   names = go t names || go k names
    go (VarT n)     names = n `elem` names
    go _            _     = False

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

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

shrink :: (a, b, c) -> (b, c)
shrink (_, b, c) = (b, c)

foldBal :: (a -> a -> a) -> a -> [a] -> a
{-# INLINE foldBal #-} -- inlined to produce specialised code for each op
foldBal op0 x0 xs0 = fold_bal op0 x0 (length xs0) xs0
  where
    fold_bal op x !n xs = case xs of
      []  -> x
      [a] -> a
      _   -> let !nl = n `div` 2
                 !nr = n - nl
                 (l,r) = splitAt nl xs
             in fold_bal op x nl l
                `op` fold_bal op x nr r

isNewtypeVariant :: DatatypeVariant_ -> Bool
isNewtypeVariant Datatype_             = False
isNewtypeVariant Newtype_              = True
isNewtypeVariant (DataInstance_ {})    = False
isNewtypeVariant (NewtypeInstance_ {}) = True

-- | Indicates whether Generic or Generic1 is being derived.
data GenericClass = Generic | Generic1 deriving Enum

-- | Records information about the type variables of a data type with a
-- 'Generic' or 'Generic1' instance.
data GenericTvbs
    -- | Information about a data type with a 'Generic' instance.
  = Gen0
      { gen0Tvbs :: [TyVarBndrUnit]
        -- ^ All of the type variable arguments to the data type.
      }
    -- | Information about a data type with a 'Generic1' instance.
  | Gen1
      { gen1InitTvbs :: [TyVarBndrUnit]
        -- ^ All of the type variable arguments to the data type except the
        --   last one. In a @'Generic1' (T a_1 ... a_(n-1))@ instance, the
        --   'gen1InitTvbs' would be @[a_1, ..., a_(n-1)]@.
      , gen1LastTvbName :: Name
        -- ^ The name of the last type variable argument to the data type.
        --   In a @'Generic1' (T a_1 ... a_(n-1))@ instance, the
        --   'gen1LastTvbName' name would be @a_n@.
     , gen1LastTvbKindVar :: Maybe Name
        -- ^ If the 'gen1LastTvbName' has kind @k@, where @k@ is some kind
        --   variable, then the 'gen1LastTvbKindVar' is @'Just' k@. Otherwise,
        --   the 'gen1LastTvbKindVar' is 'Nothing'.
      }

-- | Compute 'GenericTvbs' from a 'GenericClass' and the type variable
-- arguments to a data type.
mkGenericTvbs :: GenericClass -> [Type] -> GenericTvbs
mkGenericTvbs gClass tySynVars =
  case gClass of
    Generic  -> Gen0{gen0Tvbs = freeVariablesWellScoped tySynVars}
    Generic1 -> Gen1{ gen1InitTvbs       = freeVariablesWellScoped initArgs
                    , gen1LastTvbName    = varTToName lastArg
                    , gen1LastTvbKindVar = mbLastArgKindName
                    }
  where
    -- Everything below is only used for Generic1.
    initArgs :: [Type]
    initArgs = init tySynVars

    lastArg :: Type
    lastArg = last tySynVars

    mbLastArgKindName :: Maybe Name
    mbLastArgKindName = starKindStatusToName
                      $ canRealizeKindStar lastArg

-- | Return the type variable arguments to a data type that appear in a
-- 'Generic' or 'Generic1' instance. For a 'Generic' instance, this consists of
-- all the type variable arguments. For a 'Generic1' instance, this consists of
-- all the type variable arguments except for the last one.
genericInitTvbs :: GenericTvbs -> [TyVarBndrUnit]
genericInitTvbs (Gen0{gen0Tvbs = tvbs})     = tvbs
genericInitTvbs (Gen1{gen1InitTvbs = tvbs}) = tvbs

-- | A version of 'DatatypeVariant' in which the data family instance
-- constructors come equipped with the 'ConstructorInfo' of the first
-- constructor in the family instance (for 'Name' generation purposes).
data DatatypeVariant_
  = Datatype_
  | Newtype_
  | DataInstance_    ConstructorInfo
  | NewtypeInstance_ ConstructorInfo

showsDatatypeVariant :: DatatypeVariant_ -> ShowS
showsDatatypeVariant variant = (++ '_':label)
  where
    dataPlain :: String
    dataPlain = "Plain"

    dataFamily :: ConstructorInfo -> String
    dataFamily con = "Family_" ++ sanitizeName (nameBase $ constructorName con)

    label :: String
    label = case variant of
              Datatype_            -> dataPlain
              Newtype_             -> dataPlain
              DataInstance_    con -> dataFamily con
              NewtypeInstance_ con -> dataFamily con

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

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> Q a
etaReductionError instanceType = fail $
  "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
  ++ pprint instanceType

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: Name -> Q a
derivingKindError tyConName = fail
  . showString "Cannot derive well-kinded instance of form ‘Generic1 "
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass Generic1 expects an argument of kind "
#if MIN_VERSION_base(4,10,0)
  . showString "k -> *"
#else
  . showString "* -> *"
#endif
  $ ""

-- | The data type mentions the last type variable in a place other
-- than the last position of a data type in a constructor's field.
outOfPlaceTyVarError :: Q a
outOfPlaceTyVarError = fail
  . showString "Constructor must only use its last type variable as"
  . showString " the last argument of a data type"
  $ ""

-- | The data type mentions the last type variable in a type family
-- application.
typeFamilyApplicationError :: Q a
typeFamilyApplicationError = fail
  . showString "Constructor must not apply its last type variable"
  . showString " to an unsaturated type family"
  $ ""

-- | We cannot define implementations for @from(1)@ or @to(1)@ at the term level
-- for @type data@ declarations, which only exist at the type level.
typeDataError :: Name -> Q a
typeDataError dataName = fail
  . showString "Cannot derive instance for ‘"
  . showString (nameBase dataName)
  . showString "‘, which is a ‘type data‘ declaration"
  $ ""

-- | Cannot have a constructor argument of form (forall a1 ... an. <type>)
-- when deriving Generic(1)
rankNError :: Q a
rankNError = fail "Cannot have polymorphic arguments"

-- | Boilerplate for top level splices.
--
-- The given Name must meet one of two criteria:
--
-- 1. It must be the name of a type constructor of a plain data type or newtype.
-- 2. It must be the name of a data family instance or newtype instance constructor.
--
-- Any other value will result in an exception.
reifyDataInfo :: Name
              -> Q (Either String (Name, [Type], [ConstructorInfo], DatatypeVariant_))
reifyDataInfo name = do
  return $ Left $ ns ++ " Could not reify " ++ nameBase name
 `recover`
  do DatatypeInfo { datatypeContext   = ctxt
                  , datatypeName      = parentName
                  , datatypeInstTypes = tys
                  , datatypeVariant   = variant
                  , datatypeCons      = cons
                  } <- reifyDatatype name
     variant_ <-
       case variant of
         Datatype          -> return Datatype_
         Newtype           -> return Newtype_
         DataInstance      -> return $ DataInstance_    $ headDataFamInstCon parentName cons
         NewtypeInstance   -> return $ NewtypeInstance_ $ headDataFamInstCon parentName cons
#if MIN_VERSION_th_abstraction(0,5,0)
         Datatype.TypeData -> typeDataError parentName
#endif
     checkDataContext parentName ctxt $ Right (parentName, tys, cons, variant_)
  where
    ns :: String
    ns = "Generics.Deriving.TH.reifyDataInfo: "

    -- This isn't total, but the API requires that the data family instance have
    -- at least one constructor anyways, so this will always succeed.
    headDataFamInstCon :: Name -> [ConstructorInfo] -> ConstructorInfo
    headDataFamInstCon dataFamName cons =
      case cons of
        con:_ -> con
        [] -> error $ "reified data family instance without a data constructor: "
                   ++ nameBase dataFamName

-- | One cannot derive Generic(1) instance for anything that uses DatatypeContexts,
-- so check to make sure the Cxt field of a datatype is null.
checkDataContext :: Name -> Cxt -> a -> Q a
checkDataContext _        [] x = return x
checkDataContext dataName _  _ = fail $
  nameBase dataName ++ " must not have a datatype context"

-- | Deriving Generic(1) doesn't work with ExistentialQuantification or GADTs.
checkExistentialContext :: Name -> [TyVarBndrUnit] -> Cxt -> Q ()
checkExistentialContext constrName vars ctxt =
  unless (null vars && null ctxt) $ fail $
    nameBase constrName ++ " must be a vanilla data constructor"

#if !(MIN_VERSION_template_haskell(2,21,0)) && !(MIN_VERSION_th_abstraction(0,6,0))
type TyVarBndrVis = TyVarBndrUnit

bndrReq :: ()
bndrReq = ()
#endif

-------------------------------------------------------------------------------
-- Quoted names
-------------------------------------------------------------------------------

comp1DataName :: Name
comp1DataName = 'Comp1

infixDataName :: Name
infixDataName = 'GD.Infix

k1DataName :: Name
k1DataName = 'K1

l1DataName :: Name
l1DataName = 'L1

leftAssociativeDataName :: Name
leftAssociativeDataName = 'LeftAssociative

m1DataName :: Name
m1DataName = 'M1

notAssociativeDataName :: Name
notAssociativeDataName = 'NotAssociative

par1DataName :: Name
par1DataName = 'Par1

prefixDataName :: Name
prefixDataName = 'Prefix

productDataName :: Name
productDataName = '(:*:)

r1DataName :: Name
r1DataName = 'R1

rec1DataName :: Name
rec1DataName = 'Rec1

rightAssociativeDataName :: Name
rightAssociativeDataName = 'RightAssociative

u1DataName :: Name
u1DataName = 'U1

uAddrDataName :: Name
uAddrDataName = 'UAddr

uCharDataName :: Name
uCharDataName = 'UChar

uDoubleDataName :: Name
uDoubleDataName = 'UDouble

uFloatDataName :: Name
uFloatDataName = 'UFloat

uIntDataName :: Name
uIntDataName = 'UInt

uWordDataName :: Name
uWordDataName = 'UWord

c1TypeName :: Name
c1TypeName = ''C1

composeTypeName :: Name
composeTypeName = ''(:.:)

constructorTypeName :: Name
constructorTypeName = ''Constructor

d1TypeName :: Name
d1TypeName = ''D1

genericTypeName :: Name
genericTypeName = ''Generic

generic1TypeName :: Name
generic1TypeName = ''Generic1

datatypeTypeName :: Name
datatypeTypeName = ''Datatype

par1TypeName :: Name
par1TypeName = ''Par1

productTypeName :: Name
productTypeName = ''(:*:)

rec0TypeName :: Name
rec0TypeName = ''Rec0

rec1TypeName :: Name
rec1TypeName = ''Rec1

repTypeName :: Name
repTypeName = ''Rep

rep1TypeName :: Name
rep1TypeName = ''Rep1

s1TypeName :: Name
s1TypeName = ''S1

selectorTypeName :: Name
selectorTypeName = ''Selector

sumTypeName :: Name
sumTypeName = ''(:+:)

u1TypeName :: Name
u1TypeName = ''U1

uAddrTypeName :: Name
uAddrTypeName = ''UAddr

uCharTypeName :: Name
uCharTypeName = ''UChar

uDoubleTypeName :: Name
uDoubleTypeName = ''UDouble

uFloatTypeName :: Name
uFloatTypeName = ''UFloat

uIntTypeName :: Name
uIntTypeName = ''UInt

uWordTypeName :: Name
uWordTypeName = ''UWord

v1TypeName :: Name
v1TypeName = ''V1

conFixityValName :: Name
conFixityValName = 'conFixity

conIsRecordValName :: Name
conIsRecordValName = 'conIsRecord

conNameValName :: Name
conNameValName = 'GD.conName

datatypeNameValName :: Name
datatypeNameValName = 'GD.datatypeName

isNewtypeValName :: Name
isNewtypeValName = 'isNewtype

fromValName :: Name
fromValName = 'from

from1ValName :: Name
from1ValName = 'from1

moduleNameValName :: Name
moduleNameValName = 'moduleName

selNameValName :: Name
selNameValName = 'selName

seqValName :: Name
seqValName = 'seq

toValName :: Name
toValName = 'to

to1ValName :: Name
to1ValName = 'to1

uAddrHashValName :: Name
uAddrHashValName = 'uAddr#

uCharHashValName :: Name
uCharHashValName = 'uChar#

uDoubleHashValName :: Name
uDoubleHashValName = 'uDouble#

uFloatHashValName :: Name
uFloatHashValName = 'uFloat#

uIntHashValName :: Name
uIntHashValName = 'uInt#

uWordHashValName :: Name
uWordHashValName = 'uWord#

unComp1ValName :: Name
unComp1ValName = 'unComp1

unK1ValName :: Name
unK1ValName = 'unK1

unPar1ValName :: Name
unPar1ValName = 'unPar1

unRec1ValName :: Name
unRec1ValName = 'unRec1

trueDataName, falseDataName :: Name
trueDataName  = 'True
falseDataName = 'False

nothingDataName, justDataName :: Name
nothingDataName = 'Nothing
justDataName    = 'Just

addrHashTypeName :: Name
addrHashTypeName = ''Addr#

charHashTypeName :: Name
charHashTypeName = ''Char#

doubleHashTypeName :: Name
doubleHashTypeName = ''Double#

floatHashTypeName :: Name
floatHashTypeName = ''Float#

intHashTypeName :: Name
intHashTypeName = ''Int#

wordHashTypeName :: Name
wordHashTypeName = ''Word#

composeValName :: Name
composeValName = '(.)

errorValName :: Name
errorValName = 'error

fmapValName :: Name
fmapValName = 'fmap

undefinedValName :: Name
undefinedValName = 'undefined

decidedLazyDataName :: Name
decidedLazyDataName = 'GD.DecidedLazy

decidedStrictDataName :: Name
decidedStrictDataName = 'GD.DecidedStrict

decidedUnpackDataName :: Name
decidedUnpackDataName = 'GD.DecidedUnpack

infixIDataName :: Name
infixIDataName = 'InfixI

metaConsDataName :: Name
metaConsDataName = 'MetaCons

metaDataDataName :: Name
metaDataDataName = 'MetaData

metaSelDataName :: Name
metaSelDataName = 'MetaSel

noSourceStrictnessDataName :: Name
noSourceStrictnessDataName = 'GD.NoSourceStrictness

noSourceUnpackednessDataName :: Name
noSourceUnpackednessDataName = 'GD.NoSourceUnpackedness

prefixIDataName :: Name
prefixIDataName = 'PrefixI

sourceLazyDataName :: Name
sourceLazyDataName = 'GD.SourceLazy

sourceNoUnpackDataName :: Name
sourceNoUnpackDataName = 'GD.SourceNoUnpack

sourceStrictDataName :: Name
sourceStrictDataName = 'GD.SourceStrict

sourceUnpackDataName :: Name
sourceUnpackDataName = 'GD.SourceUnpack

packageNameValName :: Name
packageNameValName = 'packageName
