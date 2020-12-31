## `generic-deriving`: Generic programming library for generalised deriving
[![Hackage](https://img.shields.io/hackage/v/generic-deriving.svg)][Hackage: generic-deriving]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/generic-deriving.svg)](http://packdeps.haskellers.com/reverse/generic-deriving)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/dreixel/generic-deriving/workflows/Haskell-CI/badge.svg)](https://github.com/dreixel/generic-deriving/actions?query=workflow%3AHaskell-CI)

[Hackage: generic-deriving]:
  http://hackage.haskell.org/package/generic-deriving
  "generic-deriving package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This package provides functionality for generalising the deriving mechanism
in Haskell to arbitrary classes. It was first described in the paper:

* [A generic deriving mechanism for Haskell](http://dreixel.net/research/pdf/gdmh.pdf).
  Jose Pedro Magalhaes, Atze Dijkstra, Johan Jeuring, and Andres Loeh. Haskell'10.

The current implementation integrates with the new GHC Generics. See
http://www.haskell.org/haskellwiki/GHC.Generics for more information.
Template Haskell code is provided for supporting older GHCs.

This library is organized as follows:

* `Generics.Deriving.Base` defines the core functionality for GHC generics,
  including the `Generic(1)` classes and representation data types.
  On modern versions of GHC, this simply re-exports `GHC.Generics` from
  `base`. On older versions of GHC, this module backports parts of
  `GHC.Generics` that were not included at the time, including `Generic(1)`
  instances.

* `Generics.Deriving.TH` implements Template Haskell functionality for
  deriving instances of `Generic(1)`.

* Educational code: in order to provide examples of how to define and use
  `GHC.Generics`-based defaults, this library offers a number of modules
  which define examples of type classes along with default implementations
  for the classes' methods. Currently, the following modules are provided:

  * `Generics.Deriving.Copoint`

  * `Generics.Deriving.ConNames`

  * `Generics.Deriving.Enum`

  * `Generics.Deriving.Eq`

  * `Generics.Deriving.Foldable`

  * `Generics.Deriving.Functor`

  * `Generics.Deriving.Monoid`

  * `Generics.Deriving.Semigroup`

  * `Generics.Deriving.Show`

  * `Generics.Deriving.Traversable`

  * `Generics.Deriving.Uniplate`

  It is worth emphasizing that these modules are primarly intended for
  educational purposes. Many of the classes in these modules resemble other
  commonly used classes—for example, `GShow` from `Generics.Deriving.Show`
  resembles `Show` from `base`—but in general, the classes that
  `generic-deriving` defines are not drop-in replacements. Moreover, the
  generic defaults that `generic-deriving` provide often make simplifying
  assumptions that may violate expectations of how these classes might work
  elsewhere. For example, the generic default for `GShow` does not behave
  exactly like `deriving Show` would.

  If you are seeking `GHC.Generics`-based defaults for type classes in
  `base`, consider using the
  [`generic-data`](http://hackage.haskell.org/package/generic-data) library.

* `Generics.Deriving.Default` provides newtypes that allow leveraging the
  generic defaults in this library using the `DerivingVia` GHC language
  extension.

* `Generics.Deriving` re-exports `Generics.Deriving.Base`,
  `Generics.Deriving.Default`, and a selection of educational modules.
