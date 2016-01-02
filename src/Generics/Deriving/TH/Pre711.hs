{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Deriving.TH.Pre711
-- Copyright   :  (c) 2008--2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Template Haskell machinery for the proxy datatype variant of GHC generics
-- used up until GHC 7.11.
-----------------------------------------------------------------------------

module Generics.Deriving.TH.Pre711 (
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

mkDataData :: DataVariety -> Name -> Q Dec
mkDataData dv n = dataD (cxt []) (genName dv [n]) [] [] []

mkConstrData :: DataVariety -> Name -> Con -> Q Dec
mkConstrData dv dt (NormalC n _) =
  dataD (cxt []) (genName dv [dt, n]) [] [] []
mkConstrData dv dt (RecC n f) =
  mkConstrData dv dt (NormalC n (map shrink f))
mkConstrData dv dt (InfixC t1 n t2) =
  mkConstrData dv dt (NormalC n [t1,t2])
mkConstrData _ _ con = gadtError con

mkSelectData :: DataVariety -> Name -> Con -> Q [Dec]
mkSelectData dv dt (RecC n fs) = return (map one fs)
  where one (f, _, _) = DataD [] (genName dv [dt, n, f]) [] [] []
mkSelectData _ _ _ = return []

mkDataInstance :: DataVariety -> Name -> Bool -> Q Dec
mkDataInstance dv n isNewtype =
  instanceD (cxt []) (appT (conT datatypeTypeName) (mkMetaDataType dv n isNewtype)) $
    [ funD datatypeNameValName [clause [wildP] (normalB (stringE (nameBase n))) []]
    , funD moduleNameValName   [clause [wildP] (normalB (stringE name)) []]
    ]
#if __GLASGOW_HASKELL__ >= 708
 ++ if isNewtype
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

mkConstrInstance :: DataVariety -> Name -> Con -> Q Dec
mkConstrInstance dv dt (NormalC n _) = mkConstrInstanceWith dv dt n False []
mkConstrInstance dv dt (RecC    n _) = mkConstrInstanceWith dv dt n True
      [ funD conIsRecordValName [clause [wildP] (normalB (conE trueDataName)) []]]
mkConstrInstance dv dt (InfixC _ n _) =
    do
      i <- reify n
      let fi = case i of
                 DataConI _ _ _ f -> f
                 _ -> error $ "Not a data constructor name: " ++ show n
      instanceD (cxt []) (appT (conT constructorTypeName) (mkMetaConsType dv dt n False))
        [funD conNameValName   [clause [wildP] (normalB (stringE (nameBase n))) []],
         funD conFixityValName [clause [wildP] (normalB (liftFixity fi)) []]]
mkConstrInstance _ _ con = gadtError con

mkConstrInstanceWith :: DataVariety -> Name -> Name -> Bool -> [Q Dec] -> Q Dec
mkConstrInstanceWith dv dt n isRecord extra =
  instanceD (cxt []) (appT (conT constructorTypeName) (mkMetaConsType dv dt n isRecord))
    (funD conNameValName [clause [wildP] (normalB (stringE (nameBase n))) []] : extra)

mkSelectInstance :: DataVariety -> Name -> Con -> Q [Dec]
mkSelectInstance dv dt (RecC n fs) = mapM (one . fst3) fs where
  one :: Name -> Q Dec
  one f =
    instanceD (cxt []) (appT (conT selectorTypeName) (mkMetaSelType dv dt n (Just f) ()))
      [funD selNameValName [clause [wildP]
        (normalB (litE (stringL (nameBase f)))) []]]
mkSelectInstance _ _ _ = return []

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

mkMetaDataType :: DataVariety -> Name -> Bool -> Q Type
mkMetaDataType dv n _ = conT $ genName dv [n]

mkMetaConsType :: DataVariety -> Name -> Name -> Bool -> Q Type
mkMetaConsType dv dt n _ = conT $ genName dv [dt, n]

mkMetaSelType :: DataVariety -> Name -> Name -> Maybe Name -> SelStrictInfo -> Q Type
mkMetaSelType dv dt n (Just f) () = conT $ genName dv [dt, n, f]
mkMetaSelType _  _  _ Nothing  () = conT noSelectorTypeName

type SelStrictInfo = ()

reifySelStrictInfo :: Name -> [Strict] -> Q [SelStrictInfo]
reifySelStrictInfo _ bangs = return (map (const ()) bangs)
