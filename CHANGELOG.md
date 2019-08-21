# Revision history for cuckoo

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
