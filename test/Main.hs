{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019-2021 Lars Kuhtz <lakuhtz@gmail.com>
-- License: BSD3
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
-- Test functions
--
module Main
( main

-- * Testing
, test0
, test1
, test2
) where

import Control.StopWatch

import qualified Crypto.Hash.BLAKE2.BLAKE2b as C

import Data.Bool
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Unsafe as B
import Data.Hashable

import Foreign

import Numeric.Natural

import System.IO.Unsafe

import "cuckoo" System.Random.Internal

-- internal modules

import Data.Cuckoo
import Data.Cuckoo.Internal

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = do
    putStrLn "fill with random Int values up to first insert failure"
    stopWatch (test0 @Int n) >>= p "Int"

    putStrLn ""
    putStrLn "fill up to first insert failure"
    stopWatch (test1 @Int n) >>= p "Int (default instance)"
    stopWatch (test1 @Double n) >>= p "Double (default instance)"

    putStrLn ""
    putStrLn "[ByteString] fill up to first insert failure"
    stopWatch (test2 @HashablePkg n) >>= p "Hashable Package"
    stopWatch (test2 @Fnv1aSip n) >>= p "Fnv1a+Sip"
    stopWatch (test2 @Crypto n) >>= p "Blake2b_8"

    putStrLn ""
    putStrLn "[ByteString] fill to 95%"
    stopWatch (test3 @HashablePkg n) >>= p "Hashable Package"
    stopWatch (test3 @Fnv1aSip n) >>= p "Fnv1a+Sip"
    stopWatch (test3 @Crypto n) >>= p "Blake2b_8"
  where
    p l (r, t) = putStrLn $ show t <> " - " <> l <> " - " <> show r
    n = 500000

-- -------------------------------------------------------------------------- --
-- Orphans

instance CuckooFilterHash Int
instance CuckooFilterHash Double

-- -------------------------------------------------------------------------- --
-- Hash Utils

integralBytes :: Integral a => a -> B.ByteString
integralBytes = LB.toStrict . BB.toLazyByteString . BB.int64LE . fromIntegral
{-# INLINE integralBytes #-}

class Pack a where
    pack :: [Word8] -> a
    unpack :: a -> [Word8]

instance Pack B.ByteString where
    pack = B.pack
    unpack = B.unpack
    {-# INLINE pack #-}
    {-# INLINE unpack #-}

-- -------------------------------------------------------------------------- --
-- Hashable (I think, this uses SIP hash)

newtype HashablePkg = HashablePkg B.ByteString
    deriving (Show, Eq, Ord)
    deriving newtype (Semigroup, Monoid, Hashable, Pack)

instance CuckooFilterHash HashablePkg where
    cuckooHash (Salt s) a = fromIntegral $! hashWithSalt s a
    cuckooFingerprint (Salt s) a = fromIntegral $! hashWithSalt (s + 23) a
    {-# INLINE cuckooHash #-}
    {-# INLINE cuckooFingerprint #-}

-- -------------------------------------------------------------------------- --
-- Fnv1a Hashes

newtype Fnv1aSip = Fnv1aSip B.ByteString
    deriving (Show, Eq, Ord)
    deriving newtype (Semigroup, Monoid, Pack)

instance CuckooFilterHash Fnv1aSip where
    cuckooHash (Salt s) (Fnv1aSip b) = saltedFnv1aByteString s b
    cuckooFingerprint (Salt s) (Fnv1aSip b) = saltedSipHashByteString s b
    {-# INLINE cuckooHash #-}
    {-# INLINE cuckooFingerprint #-}

-- -------------------------------------------------------------------------- --
-- ByteStrings with cryptographic cuckoo filter hash functions

newtype Crypto = Crypto B.ByteString
    deriving (Show, Eq, Ord)
    deriving newtype (Semigroup, Monoid, Pack)

instance CuckooFilterHash Crypto where
    cuckooHash (Salt s) (Crypto a) = unsafeDupablePerformIO $!
        B.unsafeUseAsCStringLen h $ \(ptr, _) -> peek (castPtr ptr)
      where
        h = C.finalize 8
            $! C.update a
            $! C.update (integralBytes s)
            $! C.initialize 8

    cuckooFingerprint s a = int $ cuckooHash (s + 23) a
    {-# INLINE cuckooHash #-}
    {-# INLINE cuckooFingerprint #-}

-- -------------------------------------------------------------------------- --
-- Test

-- | Fill with random items until first insert failure
--
test0 :: forall a . Variate a => CuckooFilterHash a => Natural -> IO TestResult
test0 n = do
    rng <- initialize 0
    s <- Salt <$> uniform rng
    f <- newCuckooFilter @4 @10 @a s n
    let go i fp = do
            x <- uniform rng
            fp' <- bool fp (succ fp) <$> member f x
            insert f x >>= \case
                True -> go (succ i) fp'
                False -> return (i, fp')
    (a, b) <- go 0 0
    c <- itemCount f
    return $! TestResult a 1 b c
        (int b / int a * 100)
        (int c / int (capacityInItems f) * 100)

-- | Fill up to first insert failure
--
test1 :: forall a . CuckooFilterHash a => Num a => Natural -> IO TestResult
test1 n = do
    rng <- initialize 0
    s <- Salt <$> uniform rng
    f <- newCuckooFilter @4 @10 @a s n
    let go i fp = do
            let x = int i
            fp' <- bool fp (succ fp) <$> member f x
            insert f x >>= \case
                True -> go (succ i) fp'
                False -> return (i, fp')
    (a, b) <- go 0 0
    c <- itemCount f
    return $! TestResult a 1 b c
        (int b / int a * 100)
        (int c / int (capacityInItems f) * 100)

-- | Fill up to first insert failure
--
test2 :: forall a . Pack a => CuckooFilterHash a => Natural -> IO TestResult
test2 n = do
    rng <- initialize 0
    s <- Salt <$> uniform rng
    f <- newCuckooFilter @4 @10 @a s n
    let go i fp = do
            let x = pack (castEnum <$> show i)
            fp' <- bool fp (succ fp) <$> member f x
            insert f x >>= \case
                True -> go (succ i) fp'
                False -> return (i, fp')
    (a, b) <- go 0 0
    c <- itemCount f
    return $! TestResult a 1 b c
        (int b / int a * 100)
        (int c / int (capacityInItems f) * 100)

-- | Fill 90% of the filter
--
test3 :: forall a . Pack a => CuckooFilterHash a => Natural -> IO TestResult
test3 n = do
    rng <- initialize 0
    s <- Salt <$> uniform rng
    f <- newCuckooFilter @4 @10 @a s n
    let go i x fp
            | int i >= int @_ @Double n * 95 / 100 = return (i, x, fp)
        go i x fp = do
            let bytes = pack (castEnum <$> show i)
            fp' <- bool fp (succ fp) <$> member f bytes

            -- unless (fp == fp') $ do
            --     print $ itemHashes f bytes

            insert f bytes >>= \case
                True -> go (succ i) x fp'
                False -> go (succ i) (succ x) fp'
    (i, x, b) <- go 0 0 0
    c <- itemCount f
    return $! TestResult i x b c
        (int b / int i * 100)
        (int c / int (capacityInItems f) * 100)

data TestResult = TestResult
    { _testInsertCount :: !Int
    , _testInsertFailures :: !Int
    , _testFalsePositiveCount :: !Int
    , _testItemCount :: !Int
    , _testFalsePositiveRate :: !Double
    , _testLoadFactor :: !Double
    }
    deriving (Show, Eq, Ord)

castEnum :: Enum a => Enum b => a -> b
castEnum = toEnum . fromEnum
{-# INLINE castEnum #-}
