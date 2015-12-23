# next
* On GHC 7.11 and up, `Generics.Deriving.TH` uses the new type literal-based
  machinery
* More sensible `GEnum` instances for fixed-size integral types
* Added `GEnum Natural` instance
* Backported `Generic(1)` instances added in GHC 7.11. Specifically, `Generic`
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
