{-# LANGUAGE CPP #-}

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

import           Data.Char (isAlphaNum, ord)
import           Data.Foldable (foldr')
import           Data.List
import qualified Data.Map as Map
import           Data.Map as Map (Map)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Set (Set)

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr (pprint)
import           Language.Haskell.TH.Syntax

#ifndef CURRENT_PACKAGE_KEY
import           Data.Version (showVersion)
import           Paths_generic_deriving (version)
#endif

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

-- | Expands all type synonyms in a type. Written by Dan Rosén in the
-- @genifunctors@ package (licensed under BSD3).
expandSyn :: Type -> Q Type
expandSyn (ForallT tvs ctx t) = fmap (ForallT tvs ctx) $ expandSyn t
expandSyn t@AppT{}            = expandSynApp t []
expandSyn t@ConT{}            = expandSynApp t []
expandSyn (SigT t k)          = do t' <- expandSyn t
                                   k' <- expandSynKind k
                                   return (SigT t' k')
expandSyn t                   = return t

expandSynKind :: Kind -> Q Kind
#if MIN_VERSION_template_haskell(2,8,0)
expandSynKind = expandSyn
#else
expandSynKind = return -- There are no kind synonyms to deal with
#endif

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
                rhs' = substType subs rhs
             in expandSynApp rhs' ts''
        _ -> return $ foldl' AppT t ts
expandSynApp t ts = do
    t' <- expandSyn t
    return $ foldl' AppT t' ts

type TypeSubst = Map Name Type
type KindSubst = Map Name Kind

mkSubst :: [TyVarBndr] -> [Type] -> TypeSubst
mkSubst vs ts =
   let vs' = map tyVarBndrName vs
   in Map.fromList $ zip vs' ts

substType :: TypeSubst -> Type -> Type
substType subs (ForallT v c t) = ForallT v c $ substType subs t
substType subs t@(VarT n)      = Map.findWithDefault t n subs
substType subs (AppT t1 t2)    = AppT (substType subs t1) (substType subs t2)
substType subs (SigT t k)      = SigT (substType subs t)
#if MIN_VERSION_template_haskell(2,8,0)
                                      (substType subs k)
#else
                                      k
#endif
substType _ t                  = t

substKind :: KindSubst -> Type -> Type
#if MIN_VERSION_template_haskell(2,8,0)
substKind = substType
#else
substKind _ = id -- There are no kind variables!
#endif

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = substKind (Map.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (flip substNameWithKind starK) t ns

substTyVarBndrKind :: KindSubst -> TyVarBndr -> TyVarBndr
substTyVarBndrKind _subs (KindedTV n k) = KindedTV n $
#if MIN_VERSION_template_haskell(2,8,0)
    substKind _subs k
#else
    k
#endif
substTyVarBndrKind _ tvb = tvb

substNameWithKindStarInTyVarBndr :: Name -> TyVarBndr -> TyVarBndr
substNameWithKindStarInTyVarBndr n = substTyVarBndrKind (Map.singleton n starK)

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
#if MIN_VERSION_template_haskell(2,8,0)
                     SigT _ (VarT k) -> IsKindVar k
#endif
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
#if MIN_VERSION_template_haskell(2,8,0)
hasKindStar (SigT _ StarT) = True
#else
hasKindStar (SigT _ StarK) = True
#endif
hasKindStar _              = False

tyVarNamesOfTyVarBndr :: TyVarBndr -> [Name]
tyVarNamesOfTyVarBndr (PlainTV n)    = [n]
tyVarNamesOfTyVarBndr (KindedTV n k) = n:kindVarNamesOfKind k

-- | Gets all of the type/kind variable names mentioned somewhere in a Type.
tyVarNamesOfType :: Type -> [Name]
tyVarNamesOfType = go
  where
    go :: Type -> [Name]
    go (AppT t1 t2) = go t1 ++ go t2
    go (SigT t _k)  = go t
#if MIN_VERSION_template_haskell(2,8,0)
                           ++ go _k
#endif
    go (VarT n)     = [n]
    go _            = []

-- | Gets all of the kind variable names mentioned somewhere in a Kind.
kindVarNamesOfKind :: Kind -> [Name]
#if MIN_VERSION_template_haskell(2,8,0)
kindVarNamesOfKind = tyVarNamesOfType
#else
kindVarNamesOfKind _ = [] -- There are no kind variables
#endif

-- | Gets all of the specified type/kind variable names mentioned in a Type. In
-- contrast to 'tyVarNamesOfType', 'visibleTyVarsOfType' does not go into kinds
-- of 'SigT's.
visibleTyVarsOfType :: Type -> [TyVarBndr]
visibleTyVarsOfType = go
  where
    go :: Type -> [TyVarBndr]
    go (AppT t1 t2) = go t1 ++ go t2
    go (SigT t _)   = go t
    go (VarT n)     = [PlainTV n]
    go _            = []

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

-- | True if the type does not mention the Name
ground :: Type -> Name -> Bool
ground (AppT t1 t2) name = ground t1 name && ground t2 name
ground (SigT t _)   name = ground t name
ground (VarT t)     name = t /= name
ground ForallT{}    _    = rankNError
ground _            _    = True

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
unapplyTy :: Type -> [Type]
unapplyTy = reverse . go
  where
    go :: Type -> [Type]
    go (AppT t1 t2)    = t2 : go t1
    go (SigT t _)      = go t
    go (ForallT _ _ t) = go t
    go t               = [t]

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
#if MIN_VERSION_template_haskell(2,8,0)
uncurryKind = uncurryTy
#else
uncurryKind (ArrowK k1 k2) =
  let (kvbs, ks) = uncurryKind k2
  in (kvbs, k1:ks)
uncurryKind k = ([], [k])
#endif

tyVarBndrToType :: TyVarBndr -> Type
tyVarBndrToType (PlainTV n)    = VarT n
tyVarBndrToType (KindedTV n k) = SigT (VarT n) k

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV  name)   = name
tyVarBndrName (KindedTV name _) = name

tyVarBndrKind :: TyVarBndr -> Kind
tyVarBndrKind PlainTV{}      = starK
tyVarBndrKind (KindedTV _ k) = k

-- | If a VarT is missing an explicit kind signature, steal it from a TyVarBndr.
stealKindForType :: TyVarBndr -> Type -> Type
stealKindForType tvb t@VarT{} = SigT t (tyVarBndrKind tvb)
stealKindForType _   t        = t

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
#if MIN_VERSION_template_haskell(2,8,0)
isKindVar = isTyVar
#else
isKindVar _ = False -- There are no kind variables
#endif

-- | Returns 'True' is a 'Type' contains no type variables.
isTypeMonomorphic :: Type -> Bool
isTypeMonomorphic = go
  where
    go :: Type -> Bool
    go (AppT t1 t2) = go t1 && go t2
    go (SigT t _k)  = go t
#if MIN_VERSION_template_haskell(2,8,0)
                           && go _k
#endif
    go VarT{}       = False
    go _            = True

-- | Returns 'True' is a 'Kind' contains no kind variables.
isKindMonomorphic :: Kind -> Bool
#if MIN_VERSION_template_haskell(2,8,0)
isKindMonomorphic = isTypeMonomorphic
#else
isKindMonomorphic _ = True -- There are no kind variables
#endif

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
    go (SigT t _k)  names = go t names
#if MIN_VERSION_template_haskell(2,8,0)
                              || go _k names
#endif
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
#if MIN_VERSION_template_haskell(2,11,0)
constructorName (GadtC    names _ _)    = head names
constructorName (RecGadtC names _ _)    = head names
#endif

#if MIN_VERSION_template_haskell(2,7,0)
-- | Extracts the constructors of a data or newtype declaration.
dataDecCons :: Dec -> [Con]
dataDecCons (DataInstD _ _ _
# if MIN_VERSION_template_haskell(2,11,0)
                       _
# endif
                       cons _) = cons
dataDecCons (NewtypeInstD _ _ _
# if MIN_VERSION_template_haskell(2,11,0)
                          _
# endif
                          con _) = [con]
dataDecCons _ = error "Must be a data or newtype declaration."
#endif

-- | Indicates whether Generic or Generic1 is being derived.
data GenericClass = Generic | Generic1 deriving Enum

-- | Like 'GenericArity', but bundling two things in the 'Gen1' case:
--
-- 1. The 'Name' of the last type parameter.
-- 2. If that last type parameter had kind k (where k is some kind variable),
--    then it has 'Just' the kind variable 'Name'. Otherwise, it has 'Nothing'.
data GenericKind = Gen0
                 | Gen1 Name (Maybe Name)

-- Determines the universally quantified type variables, the types of a constructor's
-- arguments, and the last type parameter name (if there is one).
reifyConTys :: GenericClass
            -> Name
            -> Q ([TyVarBndr], [Type], GenericKind)
reifyConTys gClass conName = do
    info <- reify conName
    let (tvbs, uncTy) = case info of
          DataConI _ ty _
#if !(MIN_VERSION_template_haskell(2,11,0))
                   _
#endif
                   -> uncurryTy ty
          _ -> error "Must be a data constructor"
    let (argTys, [resTy]) = splitAt (length uncTy - 1) uncTy
    -- Make sure to expand through synonyms on the last type, or else you might
    -- have something like
    --
    --   type Constant a b = a
    --   data Good a = Good (Constant a b)
    --
    -- which you'd only be able to tell was legal if you expand Constant a b to a!
    resTyExp <- expandSyn resTy
    let numResTyVars = length . nub $ visibleTyVarsOfType resTyExp
        -- ^ We need to grab a number of type variables from the constructor's
        -- type signature to re-use for the Rep(1) type synonym's type variable
        -- binders. As it turns out, that number is equal to the number of distinct
        -- type variables which appear in the result type.
        --
        -- We assume that the visible type variables all come last in the list
        -- of forall'd type variables. I suppose nothing guarantees this, but
        -- this seems to always be the case via experimentation. Fingers crossed.
        -- TODO: This doesn't work with -XTypeInType and data families
        visibleTvbs = drop (length tvbs - numResTyVars) tvbs
    let (visibleTvbs', gk) = case gClass of
           Generic  -> (visibleTvbs, Gen0)
           Generic1 ->
             -- If deriving Generic1 and the last type variable is polykinded,
             -- make sure to substitute that kind with * in the other type
             -- variable binders' kind signatures
             let headVisibleTvbs :: [TyVarBndr]
                 lastVisibleTvb :: TyVarBndr
                 (headVisibleTvbs, [lastVisibleTvb]) =
                   splitAt (length visibleTvbs - 1) visibleTvbs

                 mbLastArgKindName :: Maybe Name
                 mbLastArgKindName = starKindStatusToName
                                   . canRealizeKindStar
                                   $ tyVarBndrToType lastVisibleTvb

                 visibleTvbsSubst :: [TyVarBndr]
                 visibleTvbsSubst =
                   case mbLastArgKindName of
                        Nothing   -> headVisibleTvbs
                        Just lakn -> map (substNameWithKindStarInTyVarBndr lakn)
                                         headVisibleTvbs
             in ( visibleTvbsSubst
                , Gen1 (tyVarBndrName lastVisibleTvb) mbLastArgKindName
                )
    return (visibleTvbs', argTys, gk)

-- | Indicates whether Generic(1) is being derived for a plain data type (DataPlain)
-- or a data family instance (DataFamily). DataFamily bundles the Name of the data
-- family instance's first constructor (for Name-generation purposes) and the types
-- used to instantiate the instance.
data DataVariety = DataPlain | DataFamily Name [Type]

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

-- | Deriving Generic(1) doesn't work with ExistentialQuantification or GADTs
gadtError :: Con -> a
gadtError con = error $
  nameBase (constructorName con) ++ " must be a vanilla data constructor"

-- | Cannot have a constructor argument of form (forall a1 ... an. <type>)
-- when deriving Generic(1)
rankNError :: a
rankNError = error "Cannot have polymorphic arguments"

-- | Cannot have a Generic(1) instance where the instance head's type is instantiated
-- to be a more "saturated" type than the original data declaration. That means
-- something like this would be rejected:
--
-- @
-- {-# LANGUAGE TypeInType #-}
-- data Hm k (a :: k) deriving Generic1
-- @
--
-- Since having a Generic1 instance would force k to be instantiated with *,
-- resulting in an instance Generic1 (Hm *) instead of instance Generic1 (Hm k).
instantiationError :: Name -> a
instantiationError tyConName = error $
    nameBase tyConName ++ " must not be instantiated"

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
        DataD ctxt _ tvbs
#if MIN_VERSION_template_haskell(2,11,0)
              _
#endif
              cons _ -> Right $
          checkDataContext name ctxt (name, False, tvbs, cons, DataPlain)
        NewtypeD ctxt _ tvbs
#if MIN_VERSION_template_haskell(2,11,0)
                 _
#endif
                 con _ -> Right $
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
                Just (DataInstD ctxt _ instTys
# if MIN_VERSION_template_haskell(2,11,0)
                                _
# endif
                                cons _) -> Right $
                  checkDataContext parentName ctxt
                    (parentName, False, tvbs, cons, DataFamily (constructorName $ head cons) instTys)
                Just (NewtypeInstD ctxt _ instTys
# if MIN_VERSION_template_haskell(2,11,0)
                                   _
# endif
                                   con _) -> Right $
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

-- | One cannot derive Generic(1) instance for anything that uses DatatypeContexts,
-- so check to make sure the Cxt field of a datatype is null.
checkDataContext :: Name -> Cxt -> a -> a
checkDataContext _        [] x = x
checkDataContext dataName _  _ = error $
  nameBase dataName ++ " must not have a datatype context"

-------------------------------------------------------------------------------
-- Manually quoted names
-------------------------------------------------------------------------------

-- By manually generating these names we avoid needing to use the
-- TemplateHaskell language extension when compiling the generic-deriving library.
-- This allows the library to be used in stage1 cross-compilers.

gdPackageKey :: String
#ifdef CURRENT_PACKAGE_KEY
gdPackageKey = CURRENT_PACKAGE_KEY
#else
gdPackageKey = "generic-deriving-" ++ showVersion version
#endif

mkGD4'4_d :: String -> Name
#if MIN_VERSION_base(4,6,0)
mkGD4'4_d = mkNameG_d "base" "GHC.Generics"
#elif MIN_VERSION_base(4,4,0)
mkGD4'4_d = mkNameG_d "ghc-prim" "GHC.Generics"
#else
mkGD4'4_d = mkNameG_d gdPackageKey "Generics.Deriving.Base.Internal"
#endif

mkGD4'9_d :: String -> Name
#if MIN_VERSION_base(4,9,0)
mkGD4'9_d = mkNameG_d "base" "GHC.Generics"
#else
mkGD4'9_d = mkNameG_d gdPackageKey "Generics.Deriving.Base.Internal"
#endif

mkGD4'4_tc :: String -> Name
#if MIN_VERSION_base(4,6,0)
mkGD4'4_tc = mkNameG_tc "base" "GHC.Generics"
#elif MIN_VERSION_base(4,4,0)
mkGD4'4_tc = mkNameG_tc "ghc-prim" "GHC.Generics"
#else
mkGD4'4_tc = mkNameG_tc gdPackageKey "Generics.Deriving.Base.Internal"
#endif

mkGD4'9_tc :: String -> Name
#if MIN_VERSION_base(4,9,0)
mkGD4'9_tc = mkNameG_tc "base" "GHC.Generics"
#else
mkGD4'9_tc = mkNameG_tc gdPackageKey "Generics.Deriving.Base.Internal"
#endif

mkGD4'4_v :: String -> Name
#if MIN_VERSION_base(4,6,0)
mkGD4'4_v = mkNameG_v "base" "GHC.Generics"
#elif MIN_VERSION_base(4,4,0)
mkGD4'4_v = mkNameG_v "ghc-prim" "GHC.Generics"
#else
mkGD4'4_v = mkNameG_v gdPackageKey "Generics.Deriving.Base.Internal"
#endif

mkGD4'9_v :: String -> Name
#if MIN_VERSION_base(4,9,0)
mkGD4'9_v = mkNameG_v "base" "GHC.Generics"
#else
mkGD4'9_v = mkNameG_v gdPackageKey "Generics.Deriving.Base.Internal"
#endif

mkBaseName_d :: String -> String -> Name
mkBaseName_d = mkNameG_d "base"

mkGHCPrimName_d :: String -> String -> Name
mkGHCPrimName_d = mkNameG_d "ghc-prim"

mkGHCPrimName_tc :: String -> String -> Name
mkGHCPrimName_tc = mkNameG_tc "ghc-prim"

comp1DataName :: Name
comp1DataName = mkGD4'4_d "Comp1"

infixDataName :: Name
infixDataName = mkGD4'4_d "Infix"

k1DataName :: Name
k1DataName = mkGD4'4_d "K1"

l1DataName :: Name
l1DataName = mkGD4'4_d "L1"

leftAssociativeDataName :: Name
leftAssociativeDataName = mkGD4'4_d "LeftAssociative"

m1DataName :: Name
m1DataName = mkGD4'4_d "M1"

notAssociativeDataName :: Name
notAssociativeDataName = mkGD4'4_d "NotAssociative"

par1DataName :: Name
par1DataName = mkGD4'4_d "Par1"

prefixDataName :: Name
prefixDataName = mkGD4'4_d "Prefix"

productDataName :: Name
productDataName = mkGD4'4_d ":*:"

r1DataName :: Name
r1DataName = mkGD4'4_d "R1"

rec1DataName :: Name
rec1DataName = mkGD4'4_d "Rec1"

rightAssociativeDataName :: Name
rightAssociativeDataName = mkGD4'4_d "RightAssociative"

u1DataName :: Name
u1DataName = mkGD4'4_d "U1"

uAddrDataName :: Name
uAddrDataName = mkGD4'9_d "UAddr"

uCharDataName :: Name
uCharDataName = mkGD4'9_d "UChar"

uDoubleDataName :: Name
uDoubleDataName = mkGD4'9_d "UDouble"

uFloatDataName :: Name
uFloatDataName = mkGD4'9_d "UFloat"

uIntDataName :: Name
uIntDataName = mkGD4'9_d "UInt"

uWordDataName :: Name
uWordDataName = mkGD4'9_d "UWord"

c1TypeName :: Name
c1TypeName = mkGD4'4_tc "C1"

composeTypeName :: Name
composeTypeName = mkGD4'4_tc ":.:"

constructorTypeName :: Name
constructorTypeName = mkGD4'4_tc "Constructor"

d1TypeName :: Name
d1TypeName = mkGD4'4_tc "D1"

genericTypeName :: Name
genericTypeName = mkGD4'4_tc "Generic"

generic1TypeName :: Name
generic1TypeName = mkGD4'4_tc "Generic1"

datatypeTypeName :: Name
datatypeTypeName = mkGD4'4_tc "Datatype"

noSelectorTypeName :: Name
noSelectorTypeName = mkGD4'4_tc "NoSelector"

par1TypeName :: Name
par1TypeName = mkGD4'4_tc "Par1"

productTypeName :: Name
productTypeName = mkGD4'4_tc ":*:"

rec0TypeName :: Name
rec0TypeName = mkGD4'4_tc "Rec0"

rec1TypeName :: Name
rec1TypeName = mkGD4'4_tc "Rec1"

repTypeName :: Name
repTypeName = mkGD4'4_tc "Rep"

rep1TypeName :: Name
rep1TypeName = mkGD4'4_tc "Rep1"

s1TypeName :: Name
s1TypeName = mkGD4'4_tc "S1"

selectorTypeName :: Name
selectorTypeName = mkGD4'4_tc "Selector"

sumTypeName :: Name
sumTypeName = mkGD4'4_tc ":+:"

u1TypeName :: Name
u1TypeName = mkGD4'4_tc "U1"

uAddrTypeName :: Name
uAddrTypeName = mkGD4'9_tc "UAddr"

uCharTypeName :: Name
uCharTypeName = mkGD4'9_tc "UChar"

uDoubleTypeName :: Name
uDoubleTypeName = mkGD4'9_tc "UDouble"

uFloatTypeName :: Name
uFloatTypeName = mkGD4'9_tc "UFloat"

uIntTypeName :: Name
uIntTypeName = mkGD4'9_tc "UInt"

uWordTypeName :: Name
uWordTypeName = mkGD4'9_tc "UWord"

v1TypeName :: Name
v1TypeName = mkGD4'4_tc "V1"

conFixityValName :: Name
conFixityValName = mkGD4'4_v "conFixity"

conIsRecordValName :: Name
conIsRecordValName = mkGD4'4_v "conIsRecord"

conNameValName :: Name
conNameValName = mkGD4'4_v "conName"

datatypeNameValName :: Name
datatypeNameValName = mkGD4'4_v "datatypeName"

isNewtypeValName :: Name
isNewtypeValName = mkGD4'4_v "isNewtype"

fromValName :: Name
fromValName = mkGD4'4_v "from"

from1ValName :: Name
from1ValName = mkGD4'4_v "from1"

moduleNameValName :: Name
moduleNameValName = mkGD4'4_v "moduleName"

selNameValName :: Name
selNameValName = mkGD4'4_v "selName"

toValName :: Name
toValName = mkGD4'4_v "to"

to1ValName :: Name
to1ValName = mkGD4'4_v "to1"

uAddrHashValName :: Name
uAddrHashValName = mkGD4'9_v "uAddr#"

uCharHashValName :: Name
uCharHashValName = mkGD4'9_v "uChar#"

uDoubleHashValName :: Name
uDoubleHashValName = mkGD4'9_v "uDouble#"

uFloatHashValName :: Name
uFloatHashValName = mkGD4'9_v "uFloat#"

uIntHashValName :: Name
uIntHashValName = mkGD4'9_v "uInt#"

uWordHashValName :: Name
uWordHashValName = mkGD4'9_v "uWord#"

unComp1ValName :: Name
unComp1ValName = mkGD4'4_v "unComp1"

unK1ValName :: Name
unK1ValName = mkGD4'4_v "unK1"

unPar1ValName :: Name
unPar1ValName = mkGD4'4_v "unPar1"

unRec1ValName :: Name
unRec1ValName = mkGD4'4_v "unRec1"

trueDataName, falseDataName :: Name
#if MIN_VERSION_base(4,4,0)
trueDataName  = mkGHCPrimName_d "GHC.Types" "True"
falseDataName = mkGHCPrimName_d "GHC.Types" "False"
#else
trueDataName  = mkGHCPrimName_d "GHC.Bool"  "True"
falseDataName = mkGHCPrimName_d "GHC.Bool"  "False"
#endif

nothingDataName, justDataName :: Name
#if MIN_VERSION_base(4,8,0)
nothingDataName = mkBaseName_d "GHC.Base"   "Nothing"
justDataName    = mkBaseName_d "GHC.Base"   "Just"
#else
nothingDataName = mkBaseName_d "Data.Maybe" "Nothing"
justDataName    = mkBaseName_d "Data.Maybe" "Just"
#endif

mkGHCPrim_tc :: String -> Name
mkGHCPrim_tc = mkNameG_tc "ghc-prim" "GHC.Prim"

addrHashTypeName :: Name
addrHashTypeName = mkGHCPrim_tc "Addr#"

charHashTypeName :: Name
charHashTypeName = mkGHCPrim_tc "Char#"

doubleHashTypeName :: Name
doubleHashTypeName = mkGHCPrim_tc "Double#"

floatHashTypeName :: Name
floatHashTypeName = mkGHCPrim_tc "Float#"

intHashTypeName :: Name
intHashTypeName = mkGHCPrim_tc "Int#"

wordHashTypeName :: Name
wordHashTypeName = mkGHCPrim_tc "Word#"

composeValName :: Name
composeValName = mkNameG_v "base" "GHC.Base" "."

errorValName :: Name
errorValName = mkNameG_v "base" "GHC.Err" "error"

fmapValName :: Name
fmapValName = mkNameG_v "base" "GHC.Base" "fmap"

undefinedValName :: Name
undefinedValName = mkNameG_v "base" "GHC.Err" "undefined"

starKindName :: Name
starKindName = mkGHCPrimName_tc "GHC.Prim" "*"

decidedLazyDataName :: Name
decidedLazyDataName = mkGD4'9_d "DecidedLazy"

decidedStrictDataName :: Name
decidedStrictDataName = mkGD4'9_d "DecidedStrict"

decidedUnpackDataName :: Name
decidedUnpackDataName = mkGD4'9_d "DecidedUnpack"

infixIDataName :: Name
infixIDataName = mkGD4'9_d "InfixI"

metaConsDataName :: Name
metaConsDataName = mkGD4'9_d "MetaCons"

metaDataDataName :: Name
metaDataDataName = mkGD4'9_d "MetaData"

metaNoSelDataName :: Name
metaNoSelDataName = mkGD4'9_d "MetaNoSel"

metaSelDataName :: Name
metaSelDataName = mkGD4'9_d "MetaSel"

noSourceStrictnessDataName :: Name
noSourceStrictnessDataName = mkGD4'9_d "NoSourceStrictness"

noSourceUnpackednessDataName :: Name
noSourceUnpackednessDataName = mkGD4'9_d "NoSourceUnpackedness"

prefixIDataName :: Name
prefixIDataName = mkGD4'9_d "PrefixI"

sourceLazyDataName :: Name
sourceLazyDataName = mkGD4'9_d "SourceLazy"

sourceNoUnpackDataName :: Name
sourceNoUnpackDataName = mkGD4'9_d "SourceNoUnpack"

sourceStrictDataName :: Name
sourceStrictDataName = mkGD4'9_d "SourceStrict"

sourceUnpackDataName :: Name
sourceUnpackDataName = mkGD4'9_d "SourceUnpack"

packageNameValName :: Name
packageNameValName = mkGD4'4_v "packageName"
