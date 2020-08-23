{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- |
Module      :  Generics.Deriving.Extra.TH.Internal
Copyright   :  (c) 2008--2009 Universiteit Utrecht
License     :  BSD3

Maintainer  :  generics@haskell.org
Stability   :  experimental
Portability :  non-portable

Template Haskell-related utilities.
-}

module Generics.Deriving.Extra.TH.Internal where

import           Control.Monad (unless)

import           Data.Char (isAlphaNum, ord)
import           Data.Foldable (foldr')
import           Data.List
import qualified Data.Map as Map
import           Data.Map as Map (Map)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Type.Equality (type (~~))

import           GHC.Exts (Addr#, Char#, Double#, Float#, Int#, Word#)
import           GHC.Generics.Extra as Generics

import qualified Language.Haskell.TH.Datatype as D
import           Language.Haskell.TH.Datatype hiding (datatypeName)
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr (pprint)
import           Language.Haskell.TH.Syntax hiding
                   ( DecidedStrictness(..), NameIs(..)
                   , SourceStrictness(..), SourceUnpackedness(..) )

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

-- | Whether a type is not of kind *, is of kind *, or is a kind variable.
data StarKindStatus = NotKindStar
                    | KindStar
                    | IsKindVar Name
  deriving Eq

-- | Does a Type have kind * or k (for some kind variable k)?
canRealizeKindStar :: Type -> StarKindStatus
canRealizeKindStar t
  | hasKindStar t = KindStar
  | otherwise = case t of
                     SigT _ (VarT k) -> IsKindVar k
                     _               -> NotKindStar

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
typeToTyVarBndr :: Type -> Maybe TyVarBndr
typeToTyVarBndr (VarT n)          = Just (PlainTV n)
typeToTyVarBndr (SigT (VarT n) k) = Just (KindedTV n k)
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

-- | Detect if a Name occurs as an argument to some type family. This makes an
-- effort to exclude /oversaturated/ arguments to type families. For instance,
-- if one declared the following type family:
--
-- @
-- type family F a :: Type -> Type
-- @
--
-- Then in the type @F a b@, we would consider @a@ to be an argument to @F@,
-- but not @b@.
isInTypeFamilyApp :: Name -> Type -> [Type] -> Q Bool
isInTypeFamilyApp name tyFun tyArgs =
  case tyFun of
    ConT tcName -> go tcName
    _           -> return False
  where
    go :: Name -> Q Bool
    go tcName = do
      info <- reify tcName
      case info of
        FamilyI (OpenTypeFamilyD (TypeFamilyHead _ bndrs _ _)) _
          -> withinFirstArgs bndrs
        FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ bndrs _ _) _) _
          -> withinFirstArgs bndrs
        _ -> return False
      where
        withinFirstArgs :: [a] -> Q Bool
        withinFirstArgs bndrs =
          let firstArgs = take (length bndrs) tyArgs
              argFVs    = freeVariables firstArgs
          in return $ name `elem` argFVs

-- | True if the type does not mention the Name
ground :: Type -> Name -> Bool
ground ty name = name `notElem` freeVariables ty

-- | Construct a type via curried application.
applyTyToTys :: Type -> [Type] -> Type
applyTyToTys = foldl' AppT

-- | Apply a type constructor name to type variable binders.
applyTyToTvbs :: Name -> [TyVarBndr] -> Type
applyTyToTvbs = foldl' (\a -> AppT a . tyVarBndrToType) . ConT

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
unapplyTy :: Type -> (Type, [Type])
unapplyTy ty = go ty ty []
  where
    go :: Type -> Type -> [Type] -> (Type, [Type])
    go _      (AppT ty1 ty2)     args = go ty1 ty1 (ty2:args)
    go origTy (SigT ty' _)       args = go origTy ty' args
    go origTy (InfixT ty1 n ty2) args = go origTy (ConT n `AppT` ty1 `AppT` ty2) args
    go origTy (ParensT ty')      args = go origTy ty' args
    go origTy _                  args = (origTy, args)

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
uncurryTy :: Type -> ([TyVarBndr], [Type])
uncurryTy (AppT (AppT ArrowT t1) t2) =
  let (tvbs, tys) = uncurryTy t2
  in (tvbs, t1:tys)
uncurryTy (SigT t _) = uncurryTy t
uncurryTy (ForallT tvbs _ t) =
  let (tvbs', tys) = uncurryTy t
  in (tvbs ++ tvbs', tys)
uncurryTy t = ([], [t])

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> ([TyVarBndr], [Kind])
uncurryKind = uncurryTy

tyVarBndrToType :: TyVarBndr -> Type
tyVarBndrToType (PlainTV n)    = VarT n
tyVarBndrToType (KindedTV n k) = SigT (VarT n) k

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
    go (SigT t _k)  = go t  && go _k
    go VarT{}       = False
    go _            = True

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- | Peel off a kind signature from a TyVarBndr (if it has one).
unKindedTV :: TyVarBndr -> TyVarBndr
unKindedTV (KindedTV n _) = PlainTV n
unKindedTV tvb            = tvb

-- | Does the given type mention any of the Names in the list?
mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t _k)  names = go t  names || go _k names
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
foldBal _  x []  = x
foldBal _  _ [y] = y
foldBal op x l   = let (a,b) = splitAt (length l `div` 2) l
                   in foldBal op x a `op` foldBal op x b

isNewtypeVariant :: DatatypeVariant_ -> Bool
isNewtypeVariant Datatype_             = False
isNewtypeVariant Newtype_              = True
isNewtypeVariant (DataInstance_ {})    = False
isNewtypeVariant (NewtypeInstance_ {}) = True

-- | Indicates whether Generic or Generic1 is being derived.
data GenericClass = Generic | Generic1 deriving Enum

-- | Like 'GenericArity', but bundling two things in the 'Gen1' case:
--
-- 1. The 'Name' of the last type parameter.
-- 2. If that last type parameter had kind k (where k is some kind variable),
--    then it has 'Just' the kind variable 'Name'. Otherwise, it has 'Nothing'.
data GenericKind = Gen0
                 | Gen1 Name (Maybe Name)

-- Determines the universally quantified type variables (possibly after
-- substituting * in the case of Generic1) and the last type parameter name
-- (if there is one).
genericKind :: GenericClass -> [Type] -> ([TyVarBndr], GenericKind)
genericKind gClass tySynVars =
  case gClass of
    Generic  -> (freeVariablesWellScoped tySynVars, Gen0)
    Generic1 -> (freeVariablesWellScoped initArgs, Gen1 (varTToName lastArg) mbLastArgKindName)
  where
    -- Everything below is only used for Generic1.
    initArgs :: [Type]
    initArgs = init tySynVars

    lastArg :: Type
    lastArg = last tySynVars

    mbLastArgKindName :: Maybe Name
    mbLastArgKindName = starKindStatusToName
                      $ canRealizeKindStar lastArg

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
  . showString "‘\n\tClass Generic1 expects an argument of kind * -> *"
  $ ""

-- | The data type mentions the last type variable in a place other
-- than the last position of a data type in a constructor's field.
outOfPlaceTyVarError :: Q a
outOfPlaceTyVarError = fail
  . showString "Constructor must only use its last type variable as"
  . showString " the last argument of a data type"
  $ ""

-- | A GADT's existential context mentions the last type variable.
tyVarInExContextError :: Q a
tyVarInExContextError = fail
  . showString "Constructor cannot use its type variable"
  . showString " in an existential context"
  $ ""

-- | Cannot have a constructor argument of form (forall a1 ... an. <type>)
-- when deriving Generic(1)
rankNError :: a
rankNError = error "Cannot have polymorphic arguments"

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
                  , D.datatypeName    = parentName
                  , datatypeInstTypes = tys
                  , datatypeVariant   = variant
                  , datatypeCons      = cons
                  } <- reifyDatatype name
     let variant_ = case variant of
                      Datatype        -> Datatype_
                      Newtype         -> Newtype_
                      -- This isn't total, but the API requires that the data
                      -- family instance have at least one constructor anyways,
                      -- so this will always succeed.
                      DataInstance    -> DataInstance_    $ head cons
                      NewtypeInstance -> NewtypeInstance_ $ head cons
     checkDataContext parentName ctxt $ Right (parentName, tys, cons, variant_)
  where
    ns :: String
    ns = "Generics.Deriving.Extra.TH.reifyDataInfo: "

-- | One cannot derive Generic(1) instance for anything that uses DatatypeContexts,
-- so check to make sure the Cxt field of a datatype is null.
checkDataContext :: Name -> Cxt -> a -> Q a
checkDataContext _        [] x = return x
checkDataContext dataName _  _ = fail $
  nameBase dataName ++ " must not have a datatype context"

-- | Deriving Generic(1) doesn't work with existentially quantified type variables.
checkExistentialVars :: Name -> [TyVarBndr] -> Q ()
checkExistentialVars constrName vars =
  unless (null vars) $ fail $
    nameBase constrName ++ " must be a vanilla data constructor"

-------------------------------------------------------------------------------
-- TH names
-------------------------------------------------------------------------------

comp1DataName :: Name
comp1DataName = 'Comp1

infixDataName :: Name
infixDataName = 'Infix

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

suchThatDataName :: Name
suchThatDataName = 'SuchThat

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

exContextTypeName :: Name
exContextTypeName = ''(:=>:)

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
conNameValName = 'conName

datatypeNameValName :: Name
datatypeNameValName = 'datatypeName

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

heqTypeName :: Name
heqTypeName = ''(~~)

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
decidedLazyDataName = 'DecidedLazy

decidedStrictDataName :: Name
decidedStrictDataName = 'DecidedStrict

decidedUnpackDataName :: Name
decidedUnpackDataName = 'DecidedUnpack

infixIDataName :: Name
infixIDataName = 'InfixI

metaConsDataName :: Name
metaConsDataName = 'MetaCons

metaDataDataName :: Name
metaDataDataName = 'MetaData

metaSelDataName :: Name
metaSelDataName = 'MetaSel

noSourceStrictnessDataName :: Name
noSourceStrictnessDataName = 'NoSourceStrictness

noSourceUnpackednessDataName :: Name
noSourceUnpackednessDataName = 'NoSourceUnpackedness

prefixIDataName :: Name
prefixIDataName = 'PrefixI

sourceLazyDataName :: Name
sourceLazyDataName = 'SourceLazy

sourceNoUnpackDataName :: Name
sourceNoUnpackDataName = 'SourceNoUnpack

sourceStrictDataName :: Name
sourceStrictDataName = 'SourceStrict

sourceUnpackDataName :: Name
sourceUnpackDataName = 'SourceUnpack

packageNameValName :: Name
packageNameValName = 'packageName
