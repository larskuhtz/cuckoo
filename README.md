[![Build Status](https://travis-ci.org/larskuhtz/cuckoo.svg?branch=master)](https://travis-ci.org/larskuhtz/cuckoo)

Haskell implementation of Cuckoo filters as described in

[B. Fan, D.G. Anderson, M. Kaminsky, M.D. Mitzenmacher. Cuckoo Filter:
Practically Better Than Bloom. In Proc. CoNEXT,
2014.](https://www.cs.cmu.edu/~dga/papers/cuckoo-conext2014.pdf)

Cuckoo filters are a data structure for probabilistic set membership. They
support insertion, deletion, and membership queries for set elements.

Membership queries may return false positive results. But queries don't return
false negative results.

Unlike Bloom filters, Cuckoo filters maintain an upper bound on the false
positive rate that is independent of the load of the filter. However, insertion
of new elements in the filter can fail. For typical configurations this
probability is very small for load factors smaller than 90 percent.

The implementation allows the user to specify the bucket size and the fingerprint
size in addition to the capacity of the filter. The user can also provide custom
functions for computing the primary hash and fingerprint.

## Installation

```bash
cabal v2-install cuckoo
```

For running the test-suites

```bash
cabal v2-test cuckoo
```

For running the benchmarks

```bash
cabal v2-bench cuckoo
```

## Example

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad (filterM)
import Data.Cuckoo
import Data.List ((\\))

-- Define CuckooFilterHash instance (this uses the default implementation)
instance CuckooFilterHash Int

main :: IO ()
main = do
    -- Create Filter for a minimum of 500000 entries
    f <- newCuckooFilter @4 @8 @Int 0 500000

    -- Insert 450000 items
    failed <- filterM (fmap not . insert f) [0..450000]

    -- Query inserted items
    missing <- filterM (fmap not . member f) [0..450000]

    -- Report results
    putStrLn $ "failed inserts: " <> show (length failed)
    putStrLn $ "FAILURE: missing " <> show (length $ missing \\ failed)
    putStrLn $ "false positives: " <> show (length $ failed \\ missing)
    c <- itemCount f
    putStrLn $ "item count: " <> show c
    putStrLn $ "capacity: " <> show (capacityInItems f)
    lf <- loadFactor f
    putStrLn $ "load factor: " <> show lf
    putStrLn $ "size in allocated bytes: " <> show (sizeInAllocatedBytes f)
```

Another example can be found the file `bench/SpellChecker.hs`.

