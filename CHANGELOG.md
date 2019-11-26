# 1.13.1 [2019.11.26]
* Backport the `Generic(1)` instances for `Kleisli` introduced in `base-4.14`.

# 1.13 [2019.08.27]
* Make `GSemigroup` a superclass of `GMonoid`. Similarly, make
  `GSemigroup'` a superclass of `GMonoid'`.
* In the instance `GMonoid (Maybe a)`, relax the constraint on `a` from
  `GMonoid` to `GSemigroup`.

# 1.12.4 [2019.04.26]
* Support `th-abstraction-0.3.0.0` or later.

# 1.12.3 [2019.02.09]
* Support `template-haskell-2.15`.
* Add a `gshowList` method to `GShow`, which lets us avoid the need for
  `OverlappingInstances` in `Generics.Deriving.TH`. As a consequence, the
  `GShow String` instance has been removed, as it is now fully subsumed by
  the `GShow [a]` instance (with which it previously overlapped).
* Functions in `Generics.Deriving.TH` now balance groups of `(:*:)` and `(:+:)`
  as much as possible (`deriving Generic` was already performing this
  optimization, and now `generic-deriving` does too).
* Add a `Generics.Deriving.Default` module demonstrating and explaining
  how and why to use `DerivingVia`. There is also a test suite with
  further examples.

# 1.12.2 [2018.06.28]
* Backport the `Generic(1)` instances for `Data.Ord.Down`, introduced in
  `base-4.12`. Add `GEq`, `GShow`, `GSemigroup`, `GMonoid`, `GFunctor`,
  `GFoldable`, `GTraversable`, and `GCopoint` instances for `Down`.
* Refactor internals using `th-abstraction`.
* Adapt to `Maybe` moving to `GHC.Maybe` in GHC 8.6.

# 1.12.1 [2018.01.11]
* Remove a test that won't work on GHC 8.4.

# 1.12 [2017.12.07]
* Adapt to the `EmptyDataDeriving` proposal (introduced in GHC 8.4):
  * `Generics.Deriving.TH` now derives `to(1)` and `from(1)` implementations
    for empty data types that are strict in the argument.
  * Introduce an `EmptyCaseOptions` field to `Options` in
    `Generics.Deriving.TH`, which controls whether generated `from(1)`/`to(1)`
    implementations for empty data types should use the `EmptyCase` extension
    or not (as is the case in GHC 8.4).
  * Add `mkFrom0Options`, `mkFrom1Options`, `mkTo0Options`, and `mkTo1Options`
    functions to `Generics.Deriving.TH`, which take `EmptyCaseOptions` as
    arguments.
  * The backported instances for `V1` are now maximally lazy, as per
    `EmptyDataDeriving`. (Previously, some instances would unnecessarily force
    their argument, such as the `Eq` and `Ord` instances.)
  * Add instances for `V1` in `Generics.Deriving.Copoint`, `.Eq`, `.Foldable`,
    `.Functor`, `.Show`, and `.Traversable`.
* Remove the bitrotting `simplInstance` function from `Generics.Deriving.TH`.

# 1.11.2 [2017.04.10]
* Add `GEq`, `GShow`, `GEnum`, and `GIx` instances for the new data types
  in `Foreign.C.Types` (`CBool`) and `System.Posix.Types` (`CBlkSize`,
  `CBlkCnt`, `CClockId`, `CFsBlkCnt`, `CFsFilCnt`, `CId`, `CKey`, and `CTimer`)
  introduced in `base-4.10.0.0`

# 1.11.1 [2016.09.10]
* Fix Template Haskell regression involving data families
* Convert examples to test suite
* Backport missing `Data` and `Typeable` instances for `Rec1`, `M1`, `(:*:)`,
  `(:+:)`, and `(:.:)`

# 1.11
* The behavior of functions in `Generics.Deriving.TH` have changed with respect
  to when type synonyms are generated for `Rep(1)` definitions. In particular:

  * By default, `deriveRepresentable(1)` will no longer define its `Rep(1)`
    type family instance in terms of the type synonym that has to be generated
    with `deriveRep(1)`. Similarly, `deriveAll(1)` and `deriveAll0And1` will no
    longer generate a type synonym. Instead, they will generate `Generic(1)`
    instances that directly define the `Rep(1)` instance inline. If you wish
    to revert to the old behavior, you will need to use the variants of those
    functions suffixed with `-Options`.
  * New functions `makeRep0Inline` and `makeRep1Inline` have been added which,
    for most purposes, should replace uses of `makeRep0`/`makeRep0FromType`
    and `makeRep1`/`makeRep1FromType` (but see the next bullet point for a
    caveat).
  * The use of `deriveRep(1)`, `makeRep0`/`makeRep0FromType`, and
    `makeRep1`/`makeRep1FromType` are now discouraged, but those functions are
    still available. The reason is that on GHC 7.0/7.2/7.4, it is impossible to use
    `makeRep0Inline`/`makeRep1Inline` due to a GHC bug. Therefore, you must use
    `makeRep0`/`makeRep1` and `deriveRep(1)` on GHC 7.0/7.2/7.4 out of necessity.

  These changes make dealing with `Generic` instances that involve `PolyKinds`
  and `TypeInType` much easier.
* All functions suffixed in `-WithKindSigs` in `Generics.Deriving.TH` have been
  removed in favor of a more sensible `-Options` suffixing scheme. The ability to
  toggle whether explicit kind signatures are used on type variable binders has
  been folded into `KindSigOptions`, which is an explicit argument to
  `deriveRep0Options`/`deriveRep1Options` and also a field in the more general
  'Options' data type.
* Furthermore, the behavior of derived instances' kind signatures has changed.
  By default, the TH code will now _always_ use explicit kind signatures
  whenever possible, regardless of whether you're working with plain data types
  or data family instances. This makes working with `TypeInType` less
  surprising, but at the cost of making it slightly more awkward to work with
  derived `Generic1` instances that constrain kinds to `*` by means of `(:.:)`.
* Since `Generic1` is polykinded on GHC 8.2 and later, the functions in
  `Generics.Deriving.TH` will no longer unify the kind of the last type
  parameter to be `*`.
* Fix a bug in which `makeRep` (and similarly named functions) would not check
  whether the argument type can actually have a well kinded `Generic(1)`
  instance.
* Backport missing `Foldable` and `Traversable` instances for `Rec1`

# 1.10.7
* Renamed internal modules to avoid using apostrophes (averting this bug:
  https://github.com/haskell/cabal/issues/3631)

# 1.10.6
* A new `base-4-9` Cabal flag was added to more easily facilitate installing
  `generic-deriving` with manually installed versions of `template-haskell`.

# 1.10.5
* Apply an optimization to generated `to(1)`/`from(1)` instances that factors out
  common occurrences of `M1`. See
  http://git.haskell.org/ghc.git/commit/9649fc0ae45e006c2ed54cc5ea2414158949fadb
* Export internal typeclass names
* Fix Haddock issues with GHC 7.8

# 1.10.4.1
* Fix Haddock parsing issue on GHC 8.0

# 1.10.4
* Backported `MonadPlus` and `MonadZip` instances for `U1`, and made the
  `Functor`, `Foldable`, `Traversable`, `Alternative`, and `Monad` instances
  for `U1` lazier to correspond with `base-4.9`

# 1.10.3
* Backported `Enum`, `Bounded`, `Ix`, `Functor`, `Applicative`, `Monad`,
 `MonadFix`, `MonadPlus`, `MonadZip`, `Foldable`, `Traversable`, and
 `Data` instances (introduced in `base-4.9`) for datatypes in the
 `Generics.Deriving.Base` module

# 1.10.2
* Fix TH regression on GHC 7.0

# 1.10.1
* Added `Generics.Deriving.Semigroup`
* Added `GMonoid` instance for `Data.Monoid.Alt`
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
