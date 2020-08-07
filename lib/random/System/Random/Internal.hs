{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: System.Random.Internal
-- Copyright: Copyright © 2019 Lars Kuhtz <lakuhtz@gmail.com>
-- License: BSD3
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
-- Dispatch for different PRNG implementations
--
module System.Random.Internal
( Gen
, Variate
, initialize
, uniform
, uniformR
) where

-- -------------------------------------------------------------------------- --
-- PCG
#ifdef RANDOM_PCG

import Control.Monad.Primitive
import System.Random.PCG hiding (initialize)
import qualified System.Random.PCG as PCG

initialize
    :: PrimMonad m
    => Int
    -> m (Gen (PrimState m))
initialize salt = PCG.initialize 0 (fromIntegral salt)

-- -------------------------------------------------------------------------- --
-- MWC
#elif defined RANDOM_MWC

import Control.Monad.Primitive
import Data.Vector
import System.Random.MWC hiding (initialize)
import qualified System.Random.MWC as MWC

initialize
    :: PrimMonad m
    => Int
    -> m (Gen (PrimState m))
initialize salt = MWC.initialize (singleton $ fromIntegral salt)

-- -------------------------------------------------------------------------- --
-- Random
#else

import Control.Monad.Primitive
import Data.STRef

#if MIN_VERSION_random(1,2,0)
import System.Random hiding (uniform, uniformR)
#else
import System.Random
#endif

type Variate a = (Random a)

type Gen s = STRef s StdGen

initialize
    :: PrimMonad m
    => Int
    -> m (Gen (PrimState m))
initialize salt = stToPrim $ newSTRef $! mkStdGen salt

uniformR
    :: Variate b
    => PrimMonad m
    => (b, b)
    -> Gen (PrimState m)
    -> m b
uniformR range gen = stToPrim $ do
    (!r, !g) <- randomR range <$> readSTRef gen
    writeSTRef gen g
    return r

uniform
    :: Variate b
    => PrimMonad m
    => Gen (PrimState m)
    -> m b
uniform gen = stToPrim $ do
    (!r, !g) <- random <$> readSTRef gen
    writeSTRef gen g
    return r

#endif

