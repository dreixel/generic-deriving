{- |
Module      :  Generics.Deriving.TH.Post4_9
Copyright   :  (c) 2008--2009 Universiteit Utrecht
License     :  BSD3

Maintainer  :  generics@haskell.org
Stability   :  experimental
Portability :  non-portable

Template Haskell machinery for the type-literal-based variant of GHC
generics introduced in @base-4.9@.
-}

module Generics.Deriving.TH.Post4_9 (
      deriveMeta
    , deriveData
    , deriveConstructors
    , deriveSelectors
    , mkMetaDataType
    , mkMetaConsType
    , mkMetaSelType
    , SelStrictInfo(..)
    , reifySelStrictInfo
  ) where

import Data.Maybe (fromMaybe)

import Generics.Deriving.TH.Internal

import Language.Haskell.TH.Datatype as THAbs
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

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
promoteStrictness THAbs.Strict          = promotedT sourceStrictDataName

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
