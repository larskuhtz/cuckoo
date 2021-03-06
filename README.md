[![Hackage](https://img.shields.io/hackage/v/cuckoo.svg?logo=haskell)](https://hackage.haskell.org/package/cuckoo)

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
cabal install cuckoo
```

For running the test-suites

```bash
cabal test cuckoo
```

For running the benchmarks

```bash
cabal bench cuckoo
```

## Example

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    failed <- filterM (fmap not . insert f) [0..500000-1]

    -- Query inserted items
    missing <- filterM (fmap not . member f) [0..500000-1]

    -- Test for false positives
    false <- filterM (member f) [500000..1000000 - 1]

    -- Report results
    putStrLn $ "failed inserts: " <> show (length failed)
    putStrLn $ "false positives: " <> show (length false)
    putStrLn $ "false positive rate (%): " <> show @Double (fromIntegral (length false) * 100 / 500000)
    putStrLn $ "missing (must be 0): " <> show (length $ missing \\ failed)

    -- Filter properties
    putStrLn $ "capacity: " <> show (capacityInItems f)
    putStrLn $ "size in allocated bytes: " <> show (sizeInAllocatedBytes f)

    -- computing the following is a bit slow
    c <- itemCount f
    putStrLn $ "item count: " <> show c
    lf <- loadFactor f
    putStrLn $ "load factor (%): " <> show lf
    putStrLn $ "bits per item: " <> show @Double (fromIntegral (sizeInAllocatedBytes f) * 8 / fromIntegral c)
```

Which produces the following results:

```bash
$ ghc -o main -threaded -O -with-rtsopts=-N Main.hs
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking main ...
$ ./main
failed inserts: 0
false positives: 14796
false postive rate (%): 2.9592
missing (must be 0): 0
capacity: 524288
size in allocated bytes: 524292
item count: 500000
load factor (%): 95.367431640625
bits per item: 8.388672
```

Another example can be found in the file
[bench/SpellChecker.hs](https://github.com/larskuhtz/cuckoo/blob/master/bench/SpellChecker.hs).

