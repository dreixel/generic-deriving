-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Deriving.TH.Post711
-- Copyright   :  (c) 2008--2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Template Haskell machinery for the type-literal-based variant of GHC
-- generics introduced in GHC 7.11.
-----------------------------------------------------------------------------

module Generics.Deriving.TH.Post711 (
      deriveMeta
    , deriveData
    , deriveConstructors
    , deriveSelectors
    , mkMetaDataType
    , mkMetaConsType
    , mkMetaSelType
    , mkMetaNoSelType
  ) where

import Generics.Deriving.TH.Internal

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

mkMetaDataType :: DataVariety -> Name -> Bool -> Q Type
mkMetaDataType _ n isNewtype =
           promotedT metaDataDataName
    `appT` litT (strTyLit (nameBase n))
    `appT` litT (strTyLit m)
    `appT` litT (strTyLit pkg)
    `appT` promoteBool isNewtype
  where
    m, pkg :: String
    m   = maybe (error "Cannot fetch module name!")  id (nameModule n)
    pkg = maybe (error "Cannot fetch package name!") id (namePackage n)

mkMetaConsType :: DataVariety -> Name -> Name -> Bool -> Q Type
mkMetaConsType _ _ n conIsRecord = do
    i <- reify n
    fi <- case i of
               DataConI{} -> reifyFixity n
               _ -> error $ "Not a data constructor name: " ++ show n
    promotedT metaConsDataName
      `appT` litT (strTyLit (nameBase n))
      `appT` fixityIPromotedType conIsRecord fi
      `appT` promoteBool conIsRecord

promoteBool :: Bool -> Q Type
promoteBool b = promotedT boolDataName
  where
    boolDataName
      | b         = trueDataName
      | otherwise = falseDataName

fixityIPromotedType :: Bool -> Fixity -> Q Type
fixityIPromotedType True (Fixity n a) =
         promotedT infixIDataName
  `appT` promoteAssociativity a
  `appT` litT (numTyLit (toInteger n))
fixityIPromotedType False _ = promotedT prefixIDataName

promoteAssociativity :: FixityDirection -> Q Type
promoteAssociativity InfixL = promotedT leftAssociativeDataName
promoteAssociativity InfixR = promotedT rightAssociativeDataName
promoteAssociativity InfixN = promotedT notAssociativeDataName

mkMetaSelType :: DataVariety -> Name -> Name -> Name -> Q Type
mkMetaSelType _ _ _ n = promotedT metaSelDataName `appT` litT (strTyLit (nameBase n))

mkMetaNoSelType :: Q Type
mkMetaNoSelType = promotedT metaNoSelDataName

-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Data' instance, the 'Constructor' instances, and the 'Selector'
-- instances.
--
-- On GHC 7.11 and up, this functionality is no longer used in GHC generics,
-- so this function generates no declarations.
deriveMeta :: Name -> Q [Dec]
deriveMeta _ = return []

-- | Given a datatype name, derive a datatype and instance of class 'Datatype'.
--
-- On GHC 7.11 and up, this functionality is no longer used in GHC generics,
-- so this function generates no declarations.
deriveData :: Name -> Q [Dec]
deriveData _ = return []

-- | Given a datatype name, derive datatypes and
-- instances of class 'Constructor'.
--
-- On GHC 7.11 and up, this functionality is no longer used in GHC generics,
-- so this function generates no declarations.
deriveConstructors :: Name -> Q [Dec]
deriveConstructors _ = return []

-- | Given a datatype name, derive datatypes and instances of class 'Selector'.
--
-- On GHC 7.11 and up, this functionality is no longer used in GHC generics,
-- so this function generates no declarations.
deriveSelectors :: Name -> Q [Dec]
deriveSelectors _ = return []
