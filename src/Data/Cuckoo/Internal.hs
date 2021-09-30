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
-- Copyright: Copyright © 2019-2021 Lars Kuhtz <lakuhtz@gmail.com>
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
) where

import Control.Monad.Primitive

import Data.Bits
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

