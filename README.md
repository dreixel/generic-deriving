## `generic-deriving`: Generic programming library for generalised deriving
[![Hackage](https://img.shields.io/hackage/v/generic-deriving.svg)][Hackage: generic-deriving]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/generic-deriving.svg)](http://packdeps.haskellers.com/reverse/generic-deriving)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/dreixel/generic-deriving.svg)](https://travis-ci.org/dreixel/generic-deriving)

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
