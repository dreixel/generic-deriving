## `generic-deriving-extra`: `GHC.Generics`, but with limited support for GADTs
[![Hackage](https://img.shields.io/hackage/v/generic-deriving-extra.svg)][Hackage: generic-deriving-extra]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/generic-deriving-extra.svg)](http://packdeps.haskellers.com/reverse/generic-deriving-extra)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/RyanGlScott/generic-deriving-extra.svg)](https://travis-ci.org/RyanGlScott/generic-deriving-extra)

[Hackage: generic-deriving-extra]:
  http://hackage.haskell.org/package/generic-deriving-extra
  "generic-deriving-extra package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

`generic-deriving-extra` is a fork of `generic-deriving` that implements
extremely rudimentary support for representing GADTs. In particular, only GADTs
with existential contexts and no existentially quantified type variables are
supported. Essentially, this library implements the techniques from
[this blog post](https://ryanglscott.github.io/2018/02/11/how-to-derive-generic-for-some-gadts/),
although this library names its representation type `(:=>:)` rather than `ECC`.
Read the blog post for a more detailed explanation of what is and isn't
supported.

Here is a description of various modules in this library:

* `Generics.Deriving.Extra.Base` exports `GHC.Generics`, the @(:=>:)@ data
  type, and `Generic(1)` instances for `(:=>:)`.

* `Generics.Deriving.Extra.TH` implements Template Haskell functionality for
  deriving instances of `Generic(1)`.

* Most of the other modules in this library (e.g.,
  `Generics.Deriving.Extra.Copoint`) re-export the corresponding modules from
  `generic-deriving`, but with additional instances defined for `(:=>:)`.

If you wish to use a datatype-generic programming library that supports a wider
range of GADTs, it is recommended that you use the `kind-generics` library.
`generic-deriving-extra` depends on `kind-apply`, a dependency of
`kind-generics`, in order to make use of its `(:=>:)` representation type, but
the approach that `kind-generics` uses to represent GADTs is otherwise quite
different than `generic-deriving-extra`'s approach, which aims to piggyback
directly on top of `GHC.Generics`.
