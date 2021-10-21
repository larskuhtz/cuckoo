{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Data.Cuckoo.Internal.HashFunctions
-- Copyright: Copyright © 2021 Lars Kuhtz <lakuhtz@gmail.com>
-- License: BSD3
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
module Data.Cuckoo.Internal.HashFunctions
(
-- * Salted Hash Functions
  saltedFnv1aPtr
, saltedFnv1aStorable
, saltedFnv1aByteString

-- ** Salted Sip Hash
, saltedSipHashPtr
, saltedSipHashStorable
, saltedSipHashByteString

-- * Internal Use Only
, sipHashInternal
) where

import qualified Data.ByteString as B
import Data.Hash.FNV1.Salted
import Data.Hash.SipHash

import Foreign

import Data.Cuckoo.Internal

-- -------------------------------------------------------------------------- --
-- FNV1a

-- | Computes a 64 bit Fnv1a hash for a value that is an instance of
-- 'BA.ByteArrayAccess'.
--
-- The first argument is use as a salt.
--
saltedFnv1aPtr
    :: Int
        -- ^ Salt
    -> Ptr Word8
        -- ^ Bytes that are hashed
    -> Int
        -- ^ Number of bytes
    -> IO Word64
saltedFnv1aPtr s p l = do
    Fnv1a64Hash !h <- hashPtr (fromIntegral s) p l
    return h
{-# INLINE saltedFnv1aPtr #-}

-- | Computes a 64 bit Fnv1a hash for a value that has an 'Storable' instance.
--
-- The first argument is use as a salt.
--
saltedFnv1aStorable
    :: Storable a
    => Int
        -- ^ Salt
    -> a
        -- ^ Value that is hashed
    -> Word64
saltedFnv1aStorable s b =
    let Fnv1a64Hash !h = hashStorable (fromIntegral s) b in h
{-# INLINE saltedFnv1aStorable #-}

saltedFnv1aByteString
    :: Int
        -- ^ Salt
    -> B.ByteString
        -- ^ Bytes that are hashed
    -> Word64
saltedFnv1aByteString s b =
    let Fnv1a64Hash !h = hashByteString (fromIntegral s) b in h
{-# INLINE saltedFnv1aByteString #-}

-- -------------------------------------------------------------------------- --
-- Sip Hash

-- | Computes a Sip hash for a value that is represented as byte pointer.
--
-- The first argument is a salt value that is used to derive the key for the
-- hash computation.
--
saltedSipHashPtr
    :: Int
        -- ^ Salt
    -> Ptr Word8
        -- ^ Bytes that is hashed
    -> Int
        -- ^ Number of bytes
    -> IO Word64
saltedSipHashPtr s ptr l = do
    SipHash !h <- hashPtr @(SipHash 2 4) (SipHashKey (int s) 1043639) ptr l
    return h
{-# INLINE saltedSipHashPtr #-}

-- | Computes a Sip hash for a value that has an 'Storable' instance.
--
-- The first argument is a salt value that is used to derive the key for the
-- hash computation.
--
saltedSipHashStorable
    :: Storable a
    => Int
        -- ^ Salt
    -> a
        -- ^ Value that is hashed
    -> Word64
saltedSipHashStorable s b =
    let SipHash !h = hashStorable @(SipHash 2 4) (SipHashKey (int s) 914279) b
    in h
{-# INLINE saltedSipHashStorable #-}

-- | Computes a Sip hash for a value that has an 'Storable' instance.
--
-- The first argument is a salt value that is used to derive the key for the
-- hash computation.
--
saltedSipHashByteString
    :: Int
        -- ^ Salt
    -> B.ByteString
        -- ^ Value that is hashed
    -> Word64
saltedSipHashByteString s b =
    let SipHash !h = hashByteString @(SipHash 2 4) (SipHashKey (int s) 914279) b
    in h
{-# INLINE saltedSipHashByteString #-}

-- -------------------------------------------------------------------------- --
-- Interal Use Only (Do Not Use)

-- | An version of a Sip hash that is used only internally. In order to avoid
-- dependencies between different hash computations, it shouldn't be used in the
-- implementation of instances of 'Data.Cuckoo.CuckooFilterHash'.
--
sipHashInternal :: Storable a => Int -> a -> Word64
sipHashInternal s b =
    let SipHash !h = hashStorable @(SipHash 2 4) (SipHashKey 994559 (int s * 713243)) b
    in h
{-# INLINE sipHashInternal #-}

