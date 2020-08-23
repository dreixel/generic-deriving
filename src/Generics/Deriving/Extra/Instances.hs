{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Deriving.Extra.Instances () where

import Generics.Deriving.Extra.TH (deriveAll0And1)
import GHC.Generics.Extra

{-
instance Generic ((c :=>: f) p) where
  type Rep ((c :=>: f) p) =
    D1
      ('MetaData
         ":=>:" "GHC.Generics.Extra" "kind-apply-???" 'False)
      (C1
         ('MetaCons
            "SuchThat"
            'PrefixI
            'False)
         (c :=>: S1
                   ('MetaSel
                      'Nothing
                      'NoSourceUnpackedness
                      'NoSourceStrictness
                      'DecidedLazy)
                   (Rec0 (f p))))
  from x = M1 (case x of SuchThat g1 -> M1 (SuchThat (M1 (K1 g1))))
  to (M1 x) = case x of M1 (SuchThat (M1 (K1 g1))) -> SuchThat g1

instance Generic1 (c :=>: f) where
  type Rep1 (c :=>: f) =
    D1
      ('MetaData
         ":=>:" "GHC.Generics.Extra" "kind-apply-???" 'False)
      (C1
         ('MetaCons
            "SuchThat"
            'PrefixI
            'False)
         (c :=>: S1
                   ('MetaSel
                      'Nothing
                      'NoSourceUnpackedness
                      'NoSourceStrictness
                      'DecidedLazy)
                   (Rec1 f)))
  from1 x = M1 (case x of SuchThat g1 -> M1 (SuchThat (M1 (Rec1 g1))))
  to1 (M1 x) = case x of M1 (SuchThat (M1 g1)) -> SuchThat (unRec1 g1)
-}
$(deriveAll0And1 ''(:=>:))
