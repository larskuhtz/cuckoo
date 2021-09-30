# Revision history for cuckoo

## 0.3.0 -- 2021-09-30

*   Rename hash functions provided hash functions to

    *   `saltedSipHashStorable`
    *   `saltedSipHashByteString`
    *   `saltedSipHashPtr`
    *   `saltedFnv1aStorable`
    *   `saltedFnv1aByteString`
    *   `saltedFnv1aPtr`

*   Use [hashes](https://hackage.haskell.org/package/hashes) package for hash
    functions and drop dependency on
    [memory](https://hackage.haskell.org/package/memory).

    The [bytestring](https://hackage.haskell.org/package/bytestring) package is
    is added as a new dependency.

*   Move implementation of hash functions from
    `Data.Cuckoo.Internal` to `Data.Cuckoo.Internal.HashFunctions`.

## 0.2.2 -- 2021-06-24

* Support GHC-9

## 0.2.1 -- 2020-08-06

* Support random-1.2.0

* Fix doctest test-suite

* Relax lower bound of primitive to 0.6.4.0

## 0.2.0.1 -- 2019-08-20

* Fixed and improved example.

## 0.2.0.0 -- 2019-08-20

* The PRNG from the random package is new the default. Added cabal flags for
  using `mwc-random` or `pcg-random` instead.

* Changed order of type parameters for `newCuckooHash`. The type of the monad is
  moved to the end, because its usually inferred from the context.

* Capacity (in items) parameter must be at least 64.

* Fixed the result of `sizeInAllocatedBytes`.

* Added examples to `Data.Cuckoo` along with a doctests test-suite.

* Various documentation fixes and improvements in `Data.Cuckoo`.

## 0.1.0.0 -- 2019-08-06

* First version. Released on an unsuspecting world.
