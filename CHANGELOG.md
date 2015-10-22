# 1.9.0
* Allow deriving of Generic1 using Template Haskell
* Allow deriving of Generic(1) for data families
* Allow deriving of Generic(1) for constructor-less plain datatypes (but not
  data families, due to technical restrictions)
* Support for unboxed representation types on GHC 7.11+
* The field accessors for the `(:+:)` type in `Generics.Deriving.Base` have
  been removed to be consistent with `GHC.Generics`
* Ensure that TH generates definitions for isNewtype and packageName, if a
  recent-enough version of GHC is used
* Ensure that TH-generated names are unique for a given data type's module and
  package (similar in spirit to Trac #10487)
* Allow building on stage-1 compilers
