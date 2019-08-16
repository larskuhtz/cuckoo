{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Main
( main

-- * checks
, prop_fit
, prop_p2
) where

import Criterion
import Criterion.Main

import Data.Bits

import Test.QuickCheck

-- internal modules

import Data.Cuckoo.Internal

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = do
    quickCheck prop_fit
    quickCheck prop_p2
    defaultMain
        [ bgroup "fit"
            [ fitBench "floating" floatingFit
            , fitBench "integral-0" integralFit_0
            , fitBench "integral-1" integralFit_1
            ]
        , bgroup "nextPowerOfTwo"
            [ p2Bench "floating" floatingP2
            , p2Bench "integral-0" integralP2_0
            , p2Bench "integral-1" integralP2_1
            , p2Bench "integral-2" integralP2_2
            ]
        ]

-- -------------------------------------------------------------------------- --
-- Fit

fitBench :: String -> (Int -> Int -> Int) -> Benchmark
fitBench l f = bgroup l
    [ bench "1" $ whnf (f 64) 8
    , bench "2" $ whnf (f 67) 4
    , bench "3" $ whnf (f 1024) 10
    , bench "4" $ whnf (f 1234567) 3456
    ]

floatingFit :: Int -> Int -> Int
floatingFit = fit
{-# INLINE floatingFit #-}

integralFit_0 :: Int -> Int -> Int
integralFit_0 a b = let (x,y) = a `divMod` b in x + signum y
{-# INLINE integralFit_0 #-}

integralFit_1 :: Int -> Int -> Int
integralFit_1 a b = 1 + (a - 1) `div` b
{-# INLINE integralFit_1 #-}

prop_fit :: Int -> Positive Int -> Property
prop_fit x (Positive y)
    = floatingFit x y === integralFit_0 x y
    .&&. floatingFit x y === integralFit_1 x y

-- -------------------------------------------------------------------------- --
-- PowerOfTwo

p2Bench :: String -> (Int -> Int) -> Benchmark
p2Bench l f = bgroup l $ go <$> [1,4,5,64,78,1232343467]
  where
    go i = bench (show i) $ whnf f i

floatingP2 :: Int -> Int
floatingP2 = nextPowerOfTwo
{-# INLINE floatingP2 #-}

-- popCount seems to profite a lot form using @-mbmi2 -msse4.2@
--
integralP2_0 :: Int -> Int
integralP2_0 0 = 1
integralP2_0 x = 1 `unsafeShiftL` (highestBit + signum (popCount x - 1))
  where
    highestBit = finiteBitSize x - countLeadingZeros x - 1
{-# INLINE integralP2_0 #-}

integralP2_1 :: Int -> Int
integralP2_1 0 = 1
integralP2_1 x = 1 `unsafeShiftL` (finiteBitSize x - countLeadingZeros (x - 1))
{-# INLINE integralP2_1 #-}

-- popCount seems to profite a lot form using @-mbmi2 -msse4.2@
--
integralP2_2 :: Int -> Int
integralP2_2 0 = 1
integralP2_2 x
    | popCount x == 1 = x
    | otherwise = 1 `unsafeShiftL` (finiteBitSize x - countLeadingZeros (x - 1))
{-# INLINE integralP2_2 #-}

-- Ignore negative values
--
prop_p2 :: NonNegative Int -> Property
prop_p2 (NonNegative x)
    = floatingP2 x === integralP2_0 x
    .&&. floatingP2 x === integralP2_1 x
    .&&. floatingP2 x === integralP2_2 x

