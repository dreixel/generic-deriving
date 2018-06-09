{-# LANGUAGE CPP #-}

{- |
Module      :  Generics.Deriving.TH.Pre4_9
Copyright   :  (c) 2008--2009 Universiteit Utrecht
License     :  BSD3

Maintainer  :  generics@haskell.org
Stability   :  experimental
Portability :  non-portable

Template Haskell machinery for the proxy datatype variant of GHC generics
used up until @base-4.9@.
-}

module Generics.Deriving.TH.Pre4_9 (
      deriveMeta
    , deriveData
    , deriveConstructors
    , deriveSelectors
    , mkMetaDataType
    , mkMetaConsType
    , mkMetaSelType
    , SelStrictInfo
    , reifySelStrictInfo
  ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import Generics.Deriving.TH.Internal

import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

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

dataInstance :: Name -> Q [Dec]
dataInstance n = do
  i <- reifyDataInfo n
  case i of
    Left  _              -> return []
    Right (n', _, _, dv) -> mkInstance n' dv
  where
    mkInstance n' dv = do
      ds <- mkDataData dv n'
      is <- mkDataInstance dv n'
      return $ [ds,is]

constrInstance :: Name -> Q [Dec]
constrInstance n = do
  i <- reifyDataInfo n
  case i of
    Left  _                 -> return []
    Right (n', _, cons, dv) -> mkInstance n' cons dv
  where
    mkInstance n' cons dv = do
      ds <- mapM (mkConstrData dv n') cons
      is <- mapM (mkConstrInstance dv n') cons
      return $ ds ++ is

selectInstance :: Name -> Q [Dec]
selectInstance n = do
  i <- reifyDataInfo n
  case i of
    Left  _                 -> return []
    Right (n', _, cons, dv) -> mkInstance n' cons dv
  where
    mkInstance n' cons dv = do
      ds <- mapM (mkSelectData dv n') cons
      is <- mapM (mkSelectInstance dv n') cons
      return $ concat (ds ++ is)

mkDataData :: DatatypeVariant_ -> Name -> Q Dec
mkDataData dv n =
  dataD (cxt []) (genName dv [n]) []
#if MIN_VERSION_template_haskell(2,11,0)
        Nothing [] (cxt [])
#else
        [] []
#endif

mkConstrData :: DatatypeVariant_ -> Name -> ConstructorInfo -> Q Dec
mkConstrData dv dt
  (ConstructorInfo { constructorName    = n
                   , constructorVars    = vars
                   , constructorContext = ctxt
                   }) = do
  checkExistentialContext n vars ctxt
  dataD (cxt []) (genName dv [dt, n]) []
#if MIN_VERSION_template_haskell(2,11,0)
        Nothing [] (cxt [])
#else
        [] []
#endif

mkSelectData :: DatatypeVariant_ -> Name -> ConstructorInfo -> Q [Dec]
mkSelectData dv dt
  (ConstructorInfo { constructorName    = n
                   , constructorVariant = cv
                   }) =
  case cv of
    NormalConstructor    -> return []
    InfixConstructor     -> return []
    RecordConstructor fs -> return (map one fs)
  where one f = DataD [] (genName dv [dt, n, f]) []
#if MIN_VERSION_template_haskell(2,11,0)
                      Nothing
#endif
                      [] []
mkDataInstance :: DatatypeVariant_ -> Name -> Q Dec
mkDataInstance dv n =
  instanceD (cxt []) (appT (conT datatypeTypeName) (mkMetaDataType dv n)) $
    [ funD datatypeNameValName [clause [wildP] (normalB (stringE (nameBase n))) []]
    , funD moduleNameValName   [clause [wildP] (normalB (stringE name)) []]
    ]
#if MIN_VERSION_base(4,7,0)
 ++ if isNewtypeVariant dv
       then [funD isNewtypeValName [clause [wildP] (normalB (conE trueDataName)) []]]
       else []
#endif
  where
    name = fromMaybe (error "Cannot fetch module name!") (nameModule n)

liftFixity :: Fixity -> Q Exp
liftFixity (Fixity n a) = conE infixDataName
    `appE` liftAssociativity a
    `appE` lift n

liftAssociativity :: FixityDirection -> Q Exp
liftAssociativity InfixL = conE leftAssociativeDataName
liftAssociativity InfixR = conE rightAssociativeDataName
liftAssociativity InfixN = conE notAssociativeDataName

mkConstrInstance :: DatatypeVariant_ -> Name
                 -> ConstructorInfo -> Q Dec
mkConstrInstance dv dt
  (ConstructorInfo { constructorName    = n
                   , constructorVars    = vars
                   , constructorContext = ctxt
                   , constructorVariant = cv
                   }) = do
  checkExistentialContext n vars ctxt
  case cv of
    NormalConstructor -> mkConstrInstanceWith dv dt n False False []
    InfixConstructor -> do
      i <- reify n
#if MIN_VERSION_template_haskell(2,11,0)
      fi <- case i of
              DataConI{} -> fromMaybe defaultFixity `fmap` reifyFixity n
#else
      let fi = case i of
                 DataConI _ _ _ f -> f
#endif
                 _ -> error $ "Not a data constructor name: " ++ show n
      mkConstrInstanceWith dv dt n False True
        [funD conFixityValName [clause [wildP] (normalB (liftFixity fi)) []]]
    RecordConstructor _ ->
      mkConstrInstanceWith dv dt n True False
        [funD conIsRecordValName [clause [wildP] (normalB (conE trueDataName)) []]]

mkConstrInstanceWith :: DatatypeVariant_
                     -> Name
                     -> Name
                     -> Bool
                     -> Bool
                     -> [Q Dec]
                     -> Q Dec
mkConstrInstanceWith dv dt n isRecord isInfix extra =
  instanceD
    (cxt [])
    (appT (conT constructorTypeName) (mkMetaConsType dv dt n isRecord isInfix))
    (funD conNameValName [clause [wildP] (normalB (stringE (nameBase n))) []] : extra)

mkSelectInstance :: DatatypeVariant_ -> Name
                 -> ConstructorInfo -> Q [Dec]
mkSelectInstance dv dt
  (ConstructorInfo { constructorName    = n
                   , constructorVariant = cv
                   }) =
  case cv of
    NormalConstructor    -> return []
    InfixConstructor     -> return []
    RecordConstructor fs -> mapM one fs
  where
  one :: Name -> Q Dec
  one f =
    instanceD (cxt []) (appT (conT selectorTypeName) (mkMetaSelType dv dt n (Just f) ()))
      [funD selNameValName [clause [wildP]
        (normalB (litE (stringL (nameBase f)))) []]]

genName :: DatatypeVariant_ -> [Name] -> Name
genName dv ns
  = mkName
  . showsDatatypeVariant dv
  . intercalate "_"
  . consQualName
  $ map (sanitizeName . nameBase) ns
  where
    consQualName :: [String] -> [String]
    consQualName = case ns of
        []  -> id
        n:_ -> (showNameQual n :)

mkMetaDataType :: DatatypeVariant_ -> Name -> Q Type
mkMetaDataType dv n = conT $ genName dv [n]

mkMetaConsType :: DatatypeVariant_ -> Name -> Name -> Bool -> Bool -> Q Type
mkMetaConsType dv dt n _ _ = conT $ genName dv [dt, n]

mkMetaSelType :: DatatypeVariant_ -> Name -> Name -> Maybe Name
              -> SelStrictInfo -> Q Type
mkMetaSelType dv dt n (Just f) () = conT $ genName dv [dt, n, f]
mkMetaSelType _  _  _ Nothing  () = conT noSelectorTypeName

type SelStrictInfo = ()

reifySelStrictInfo :: Name -> [FieldStrictness] -> Q [SelStrictInfo]
reifySelStrictInfo _ bangs = return (map (const ()) bangs)
