{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- module: main
-- copyright: copyright © 2019 lars kuhtz <lakuhtz@gmail.com>
-- license: mit
-- maintainer: lars kuhtz <lakuhtz@gmail.com>
-- stability: experimental
--
-- This code is an adaptation of the respective code from Chris Coffey's cuckoo filter package,
-- which in turn is borrowed from Bryan O'Sullivan's bloom filter package.
--
module Main
( main
, runSpellCheck
) where

import Control.Monad (filterM, unless)
import Control.StopWatch

import Data.ByteArray ()
import qualified Data.ByteString.Char8 as B
import qualified Data.Cuckoo as C
import Data.List ((\\))

import Prelude hiding (words)

-- Just changing the salt isn't enough to make the hash functions independent
-- enough.
--
-- instance C.CuckooFilterHash B.ByteString where
--     cuckooHash (C.Salt s) a = fromIntegral $! hashWithSalt s a
--     cuckooFingerprint (C.Salt s) a = fromIntegral $! hashWithSalt (s + 23) a
--     {-# INLINE cuckooHash #-}
--     {-# INLINE cuckooFingerprint #-}

instance C.CuckooFilterHash B.ByteString where
    cuckooHash (C.Salt s) = C.fnv1a_bytes s
    cuckooFingerprint (C.Salt s) = C.sip_bytes s
    {-# INLINE cuckooHash #-}
    {-# INLINE cuckooFingerprint #-}

main :: IO ()
main = runSpellCheck

dict :: String
dict = "/usr/share/dict/words"

runSpellCheck :: IO ()
runSpellCheck = do
    -- read and count words
    (words, t0) <- stopWatch $ do
        words <- B.lines `fmap` B.readFile dict
        putStrLn $ show (length words) ++ " words"
        return words

    putStrLn $ show t0 <> "s to count words"

    ((f, failed), t1) <- stopWatch $ do
        f <- C.newCuckooFilter @IO @4 @8 0 500000
        failed <- filterM (fmap not . C.insert f) words
        return (f, failed)

    putStrLn $ show t1 ++ "s to construct filter"

    -- check words
    (missing, t2) <- stopWatch $
        filterM (fmap not . C.member f) words

    putStrLn $ show t2 ++ "s to query every element"

    -- report results

    unless (null failed) $
        putStrLn $ "failed inserts: " <> show (length failed)

    let unexpectedMissing = missing \\ failed
    unless (null $ unexpectedMissing) $
        putStrLn $ "FAILURE: missing " <> show (length unexpectedMissing)

    let falsePositives = failed \\ missing
    unless (null $ falsePositives) $
        putStrLn $ "false positives: " <> show (length falsePositives)

    lf <- C.loadFactor f
    putStrLn $ "load factor: " <> show lf

