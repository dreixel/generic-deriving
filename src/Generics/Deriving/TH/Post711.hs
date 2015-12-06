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

module Generics.Deriving.TH.Post711 where

import Generics.Deriving.TH.Internal
import Language.Haskell.TH.Syntax

mkMetaDataType :: Name -> Bool -> Q Type
mkMetaDataType n isNewtype =
           promotedT metaDataDataName
    `appT` litT (strTyLit (nameBase n))
    `appT` litT (strTyLit m)
    `appT` litT (strTyLit pkg)
    `appT` promoteBool isNewtype
  where
    m, pkg :: String
    m   = maybe (error "Cannot fetch module name!")  id (nameModule n)
    pkg = maybe (error "Cannot fetch package name!") id (namePackage n)

mkMetaConsType :: Name -> Bool -> Q Type
mkMetaConsType n conIsRecord = do
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
    promoteBool
      | b         = trueDataName
      | otherwise = falseDataName

fixityIPromotedType :: Bool -> Fixity -> Q Type
fixityIPromotedType True (Fixity n a) =
         promotedT infixIDataName
  `appT` promoteAssociativity a
  `appT` litT (intTyLit n)
fixityIPromotedType False _ = promotedT prefixIDataName

promoteAssociativity :: FixityDirection -> Q Type
promoteAssociativity InfixL = promotedT leftAssociativeDataName
promoteAssociativity InfixR = promotedT rightAssociativeDataName
promoteAssociativity InfixN = promotedT notAssociativeDataName

mkMetaSelType :: Name -> Q Type
mkMetaSelType n = promotedT metaSelDataName `appT` litT (strTyLit (nameBase n))

mkMetaNoSelType :: Q Type
mkMetaNoSelType = promotedT metaNoSelDataName

deriveMeta :: Name -> Q [Dec]
deriveMeta _ = return []
