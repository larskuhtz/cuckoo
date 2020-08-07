{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Data.Cuckoo.Internal
-- Copyright: Copyright © 2019 Lars Kuhtz <lakuhtz@gmail.com>
-- License: BSD3
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
-- Internal Utilities. No guarantee is made about the stability of these
-- functions. Changes to these function won't be announced in the CHANGELOG and
-- are not reflected in the package version.
--
module Data.Cuckoo.Internal
( w
, int
, fit
, intFit
, nextPowerOfTwo
, intNextPowerOfTwo
, set
, get

-- * Hash functions
, sip
, fnv1a
, fnv1a_bytes
, sip_bytes
, sip2
) where

import Control.Monad.Primitive

import Data.Bits
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Hash as BA
import qualified Data.ByteArray.Pack as BA
import Data.Primitive.ByteArray

import Foreign

import GHC.Exts
import GHC.TypeLits

-- | Reify type level 'Nat' into 'Int' value.
--
w :: forall (n :: Nat) . KnownNat n => Int
w = int $ natVal' @n proxy#
{-# INLINE w #-}

-- | An shorter alias for 'fromIntegral'.
--
int :: Integral a => Num b => a -> b
int = fromIntegral
{-# INLINE int #-}

-- | @fit a b@ computes how many @b@s are needed to fit @a@, i.e.
-- \(\left\lceil\frac{a}{b}\right\rceil\).
--
-- For instance,
--
-- >>> fit 7 3
-- 3
--
-- >>> fit 6 3
-- 2
--
fit :: Real a => Real b => Integral c => a -> b -> c
fit a b = ceiling @Double $ realToFrac a / realToFrac b
{-# INLINE fit #-}

-- | @fit a b@ computes how many @b@s are needed to fit @a@, i.e.
-- \(\left\lceil\frac{a}{b}\right\rceil\).
--
-- For instance,
--
-- >>> intFit 7 3
-- 3
--
-- >>> intFit 6 3
-- 2
--
intFit :: Integral a => Integral b => a -> b -> a
intFit a b = 1 + (a - 1) `div` int b
{-# INLINE intFit #-}

-- | @nextPowerOfTwo a@ computes the smallest power of two that is larger or
-- equal than @a@.
--
nextPowerOfTwo :: Real a => Integral b => a -> b
nextPowerOfTwo x = 2 ^ ceiling @Double @Int (logBase 2 $ realToFrac x)
{-# INLINE nextPowerOfTwo #-}

-- | @nextPowerOfTwo a@ computes the smallest power of two that is larger or
-- equal than @a@.
--
intNextPowerOfTwo :: Int -> Int
intNextPowerOfTwo 0 = 1
intNextPowerOfTwo x = 1 `unsafeShiftL` (finiteBitSize x - countLeadingZeros (x - 1))
{-# INLINE intNextPowerOfTwo #-}

-- | Computes a 64 bit Fnv1a hash for a value that has an 'Storable' instance.
--
-- The first argument is use as a salt.
--
fnv1a
    :: Storable a
    => Int
        -- ^ Salt
    -> a
        -- ^ Value that is hashes
    -> Word64
fnv1a s x = r
  where
    Right (BA.FnvHash64 r) = BA.fnv1a_64Hash
        <$> BA.fill @BA.Bytes (8 + sizeOf x) (BA.putStorable s >> BA.putStorable x)
{-# INLINE fnv1a #-}

-- | Computes a 64 bit Fnv1a hash for a value that is an instance of
-- 'BA.ByteArrayAccess'.
--
-- The first argument is use as a salt.
--
fnv1a_bytes
    :: BA.ByteArrayAccess a
    => Int
        -- ^ Salt
    -> a
        -- ^ Value that is hashes
    -> Word64
fnv1a_bytes s x = r
  where
    Right (BA.FnvHash64 r) = BA.fnv1a_64Hash
        <$> BA.fill @BA.Bytes (8 + BA.length x) (BA.putStorable s >> BA.putBytes x)
{-# INLINE fnv1a_bytes #-}

-- | Computes a Sip hash for a value that has an 'Storable' instance.
--
-- The first argument is a salt value that is used to derive the key for the
-- hash computation.
--
sip
    :: Storable a
    => Int
        -- ^ Salt
    -> a
        -- ^ Value that is hashes
    -> Word64
sip s x = r
  where
    Right (BA.SipHash r) = BA.sipHash (BA.SipKey (int s) 914279)
        <$> BA.fill @BA.Bytes (sizeOf x) (BA.putStorable x)
{-# INLINE sip #-}

-- | Computes a Sip hash for a value that is an instance of
-- 'BA.ByteArrayAccess'.
--
-- The first argument is a salt value that is used to derive the key for the
-- hash computation.
--
sip_bytes
    :: BA.ByteArrayAccess a
    => Int
        -- ^ Salt
    -> a
        -- ^ Value that is hashes
    -> Word64
sip_bytes s x = r
  where
    Right (BA.SipHash r) = BA.sipHash (BA.SipKey (int s) 1043639)
        <$> BA.fill @BA.Bytes (BA.length x) (BA.putBytes x)
{-# INLINE sip_bytes #-}

-- | An version of a Sip hash that is used internally. In order to avoid
-- dependencies between different hash computations, it shouldn't be used in the
-- implementation of instances of 'Data.Cuckoo.CuckooFilterHash'.
--
sip2 :: Storable a => Int -> a -> Word64
sip2 s x = r
  where
    Right (BA.SipHash r) = BA.sipHash (BA.SipKey 994559 (int s * 713243))
        <$> BA.fill @BA.Bytes (sizeOf x) (BA.putStorable x)
{-# INLINE sip2 #-}

-- | Write a 'Word64' value into a 'Word32' aligned 'MutableByteArray'
--
set
    :: PrimMonad m
    => MutableByteArray (PrimState m)
    -> Int
        -- ^ index in terms of 'Word32'
    -> Word64
        -- ^ 'Word64' value that is written
    -> m ()
set x i c = do
    writeByteArray @Word32 x i (int c)
    writeByteArray @Word32 x (succ i) (int $ c `unsafeShiftR` 32)
{-# INLINE set #-}

-- | Get a 'Word64' from a 'Word32' aligned 'MutableByteArray'.
--
get
    :: PrimMonad m
    => MutableByteArray (PrimState m)
        -- ^ byte array
    -> Int
        -- ^ index in terms of 'Word32'
    -> m Word64
        -- ^ Word64 value that contains the result bits
get x i = do
    a <- readByteArray @Word32 x i
    b <- readByteArray @Word32 x (succ i)

    -- TODO check of for host byte order
    -- Here we assume littel endian
    return $! int a + (int b `unsafeShiftL` 32)
{-# INLINE get #-}

