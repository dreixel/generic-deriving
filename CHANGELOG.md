# next
* Fixed a bug in the `GEnum` instances for unsigned `Integral` types
* Added `Safe`/`Trustworthy` pragmas
* Made instances polykinded where possible

# 1.10.0
* On GHC 8.0 and up, `Generics.Deriving.TH` uses the new type literal-based
  machinery
* Rewrote the Template Haskell code to be robust. Among other things, this fixes
  a bug with deriving Generic1 instances on GHC 7.8, and makes it easier to
  derive Generic1 instances for datatypes that utilize GHC 8.0's `-XTypeInType`
  extension.
* Added `deriveAll0` and `makeRep0` for symmetry with `deriveAll1` and
  `makeRep1`
* Added`makeRep0FromType` and `makeRep1FromType` to make it easier to pass
  in the type instance (instead of having to pass each individual type
  variable, which can be error-prone)
* Added functions with the suffix `-WithKindSigs` to allow generating type
  synonyms with explicit kind signatures in the presence of kind-polymorphic
  type variables. This is necessary for some datatypes that use
  `-XTypeInType` to have derived `Generic(1)` instances, but is not turned on
  by default since the TH kind inference is not perfect and would cause
  otherwise valid code to be rejected. Use only if you know what you are doing.
* Fixed bug where a datatype with a single, nullary constructor would generate
  incorrect `Generic` instances
* More sensible `GEnum` instances for fixed-size integral types
* Added `GCopoint`, `GEnum`, `GEq`, `GFoldable`, `GFunctor`, `GMonoid`,
  `GShow`, and `GTraversable` instances for datatypes introduced in GHC 8.0
* Backported `Generic(1)` instances added in GHC 8.0. Specifically, `Generic`
  instances for `Complex` (`base-4.4` and later) `ExitCode`, and `Version`; and
  `Generic1` instances for `Complex` (`base-4.4` and later) and `Proxy`
  (`base-4.7` and later). Added `GEnum`, `GEq`, `GFoldable`, `GFunctor`, `GIx`,
  `GShow`, and `GTraversable` instances for these datatypes where appropriate.

# 1.9.0
* Allow deriving of Generic1 using Template Haskell
* Allow deriving of Generic(1) for data families
* Allow deriving of Generic(1) for constructor-less plain datatypes (but not
  data families, due to technical restrictions)
* Support for unboxed representation types on GHC 7.11+
* More `GCopoint`, `GEnum`, `GEq`, `GFoldable`, `GFunctor`, `GIx`, `GMonoid`,
  `GShow`, and `GTraversable` instances
* The field accessors for the `(:+:)` type in `Generics.Deriving.Base` have
  been removed to be consistent with `GHC.Generics`
* Ensure that TH generates definitions for isNewtype and packageName, if a
  recent-enough version of GHC is used
* Ensure that TH-generated names are unique for a given data type's module and
  package (similar in spirit to Trac #10487)
* Allow building on stage-1 compilers
