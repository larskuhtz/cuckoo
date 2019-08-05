{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Data.Cuckoo
-- Copyright: Copyright © 2019 Lars Kuhtz <lakuhtz@gmail.com>
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
-- Cuckoo Filters
--
module Data.Cuckoo
(
-- * Hash Functions
  Salt(..)
, CuckooFilterHash(..)

-- ** Hash functions
, sip
, sip_bytes
, fnv1a
, fnv1a_bytes

-- * Cuckoo Filter
, CuckooFilter
, CuckooFilterIO
, newCuckooFilter

-- * Cuckoo Filter Operations
, insert
, member
, delete

-- * Utils
, sizeInAllocatedBytes
, capacityInItems
, itemCount
, loadFactor

-- * Debugging Utils
, showFilter
, itemHashes
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive

import Data.Bits
import Data.Bool
import Data.Kind
import Data.Maybe
import Data.Primitive.ByteArray
import qualified Data.Vector as V

import Foreign

import GHC.TypeLits

import Numeric.Natural

import Prelude hiding (null)

import qualified System.Random.MWC as MWC

import Text.Printf

-- internal modules

import Data.Cuckoo.Internal

-- -------------------------------------------------------------------------- --
-- Hash Functions

-- The hashable package is a bit of a kitchen sink. Instances for different data
-- types use hash functions with very different properites and of varying
-- quality. Neither of this is documented.
--
-- Primitive base types, including all number types use fast instances that
-- provided very little uniformity in the output with respect to input data and
-- salt. Don't use these!
--
-- ByteString uses a pure Haskell implementation of Sip hash.
--
-- The helper functions for low-level ptrs (Ptr, ByteArray) use a C
-- implementation of fnv.
--
-- Because of the variying quality and properties of the functions, absence of
-- any control over which function is use, and no guarantees with respect to
-- stability accross versions, we don't use that package altogether.
--

-- | Salt for hash computations.
--
newtype Salt = Salt Int
    deriving (Show, Eq, Ord, Enum, Integral, Real, Num)

-- | Choosing good hash functions is imperative for a good performance of a
-- cuckoo filter. The hash functions must be
--
-- * independent and
-- * provide good uniformity on the lower bits of the output.
--
-- The default implementations use sip hash for 'cuckooHash' and 'fnv1a' (64
-- bit) for 'cuckooFingerprint'.
--
class CuckooFilterHash a where

    -- | This function must provide good entropy on the lower
    -- @2^bucketnumber - 1@ bits of the result.
    --
    cuckooHash :: Salt -> a -> Word64

    -- | This function must provide good entropy on the lower
    -- bits of the size of a fingerprint.
    --
    cuckooFingerprint :: Salt -> a -> Word64

    default cuckooHash :: Storable a => Salt -> a -> Word64
    cuckooHash (Salt s) a = sip s a
    {-# INLINE cuckooHash #-}

    default cuckooFingerprint :: Storable a => Salt -> a -> Word64
    cuckooFingerprint (Salt s) a = fnv1a s a
    {-# INLINE cuckooFingerprint #-}

-- -------------------------------------------------------------------------- --
-- Cuckoo Filter

-- | Cuckoo Filter with
--
-- * State token @s@,
-- * bucket size @b@,
-- * fingerprint size @f@, and
-- * and content types @a@.
--
-- The following constraints apply
--
-- * \(0 < f \leq 32\)
-- * \(0 < b\)
--
-- The implementation is not thread safe. For concurrent use the filter must be
-- wrapped in a read-write lock.
--
data CuckooFilter s (b :: Nat) (f :: Nat) (a :: Type)
    = CuckooFilter
        { _cfBucketCount :: {-# UNPACK #-} !Int
        , _cfSalt :: {-# UNPACK #-} !Salt
        , _cfRng :: {-# UNPACK #-} !(MWC.Gen s)
        , _cfData :: {-# UNPACK #-} !(MutableByteArray s)
        }

-- | Cuckoo filter that can be used in the `IO` monad.
--
type CuckooFilterIO b f a = CuckooFilter RealWorld b f a

-- | Create a new Cuckoo filter that has at least the given capacity.
--
-- * State token @s@,
-- * bucket size @b@,
-- * fingerprint size @f@, and
-- * and content types @a@.
--
-- The following constraints apply
--
-- * \(0 < f \leq 32\)
-- * \(0 < b\)
--
-- The false positive rate depends mostly on the value of @f@. It is bounded
-- from above by \(2b / 2^f\). In most cases @4@ is a good choice for @b@.
--
-- Actual performance depends on the choice of good hash functions that provide
-- high uniformity on the lower bits.
--
-- The actual capacity may be much larger than what is requested, because the
-- actual bucket count is a power of two.
--
newCuckooFilter
    :: forall m b f a
    . KnownNat b
    => KnownNat f
    => PrimMonad m
    => Salt
        -- ^ Salt for the hash functions
    -> Natural
        -- ^ Size (must be positive)
    -> m (CuckooFilter (PrimState m) b f a)
newCuckooFilter salt n = do
    check
    arr <- newByteArray bytes
    fillByteArray arr 0 bytes 0
    CuckooFilter buckets salt
        <$> MWC.initialize (V.singleton $ int salt)
        <*> pure arr
  where
    minBuckets = fit n (w @b) -- minimum number of buckets match requested capacity
    buckets = nextPowerOfTwo @Int minBuckets -- actual number of buckets
    items = w @b * buckets -- actual capacity (in number of items)
    bytes = fit @Int @Int (w @f * items) 8 + 4 -- total number of allocated bytes

        -- we add 4 extra bytes to avoid having to deal with corner cases when
        -- reading and writing fingerprints that are not aligned to Word32 at
        -- the end of the filter.

    check
        | not (0 < w @f) = error "Fingerprint size must be positive"
        | not (w @f <= 32) = error "Fingerprint size must not be larger than 32"
        | not (0 < w @b) = error "Bucket size (items per bucket) must be positive"
        | not (0 < n) = error "The size (number of items) of the filter must be positive"
        | not (32 <= int n * w @f) = error "Seriously? Are you kidding me? If you need to represent such a tiny set, you'll have to pick another data structure for that"
        | otherwise = return ()

-- -------------------------------------------------------------------------- --
-- Insert

-- TODO: reduce number of reads, in checkBucket and don't re-read value
-- during setFingerprint. Similarly, don't recompute hashes.
--
-- TODO: is the RNG really important here (for security or performance) or is
-- the hash function sufficient? (For preventing attacks, is the salt
-- sufficient). Could we also compute the relocation slot from the hash?

-- | Insert an item into the filter and return whether the operation was
-- successful. If insertion fails, the filter is unchanged.
--
-- This function is not thread safe. No concurrent writes or reads should occur
-- while this function is executed. If this is needed a lock must be used.
--
-- This function is not exception safe. The filter must not be used any more
-- after an asynchronous exception has been throw during the computation of this
-- function. If this function is used in the presence of asynchronous exceptions
-- it should be apprioriately masked.
--
insert
    :: forall m b f a
    . KnownNat f
    => KnownNat b
    => PrimMonad m
    => CuckooFilterHash a
    => CuckooFilter (PrimState m) b f a
    -> a
    -> m Bool
insert f a = do
  (b1, b2, fp) <- getBucketsRandom f a
  checkBucket f b1 null >>= \case
      Just i -> True <$ setFingerprint f b1 i fp
      Nothing -> kick 500 b2 fp
  where

    -- TODO make this exception safe? Do we need that?
    --
    kick 0 _ _ = return False
    kick c b k = checkBucket f b null >>= \case
        Just i -> True <$ setFingerprint f b i k
        Nothing -> do
            i <- randomSlot
            k' <- swapFingerprint b i k
            kick (pred @Int c) (otherBucket f b k') k' >>= \case
                False -> False <$ setFingerprint f b i k'
                x -> return x
    {-# INLINE kick #-}

    randomSlot = Slot <$> MWC.uniformR (0, w @b - 1) (_cfRng f)
    {-# INLINE randomSlot #-}

    swapFingerprint b i k = do
        k' <- readFingerprint f b i
        setFingerprint f b i k
        return k'
    {-# INLINE swapFingerprint #-}

-- -------------------------------------------------------------------------- --
-- Member Test

-- | Test whether an item is in the set that is represented by the Cuckoo
-- filter.
--
-- A negative result means that the item is definitively not in the set. A
-- positive result means that the item is most likely in the set. The rate of
-- false positives is bounded from above by \(2b / 2^f\) where @b@ is the number
-- of items per bucket and @f@ is the size of a fingerprint in bits.
--
member
    :: CuckooFilterHash a
    => PrimMonad m
    => KnownNat f
    => KnownNat b
    => CuckooFilter (PrimState m) b f a
    -> a
    -> m Bool
member f a = checkBucket f b1 fp >>= \case
    Just _ -> return True
    Nothing -> checkBucket f b2 fp >>= \case
        Just _ -> return True
        Nothing -> return False
  where
    salt = _cfSalt f
    b1 = bucket1 f a
    b2 = otherBucket f b1 fp
    fp = mkFingerprint salt a
{-# INLINE member #-}

-- -------------------------------------------------------------------------- --
-- Delete

-- | Delete an items from the filter.
--
-- /IMPORTANT/ An item must only be deleted if it was successfully added to the
-- filter before (and hasn't been deleted since then).
--
-- Deleting an item that isn't in the filter will result in the filter returning
-- false negative results.
--
-- This function is not thread safe. No concurrent writes must occur while this
-- function is executed. If this is needed a lock must be used. Concurrent reads
-- are fine.
--
delete
    :: CuckooFilterHash a
    => PrimMonad m
    => KnownNat f
    => KnownNat b
    => CuckooFilter (PrimState m) b f a
    -> a
    -> m Bool
delete f a = do
    (b1, b2, fp) <- getBucketsRandom f a
    checkBucket f b1 fp >>= \case
        Just i -> True <$ setFingerprint f b1 i null
        Nothing -> checkBucket f b2 fp >>= \case
            Just i -> True <$ setFingerprint f b2 i null
            Nothing -> return False

-- -------------------------------------------------------------------------- --
-- Internal
-- -------------------------------------------------------------------------- --

newtype Fingerprint (f :: Nat) = Fingerprint Word64
    deriving (Show, Eq, Ord)

newtype Bucket = Bucket Int
    deriving (Show, Eq, Ord, Enum, Integral, Real, Num)

newtype Slot = Slot Int
    deriving (Show, Eq, Ord, Enum, Integral, Real, Num)

-- TODO: Should we expose this function, too, in 'CuckooFilterHash'? By hiding
-- it here there is some chance that a user accidentally picks a function that
-- isn't independent from this one.
--
hashFingerprint :: Salt -> Fingerprint f -> Int
hashFingerprint (Salt s) (Fingerprint a) = int $! sip2 s a
{-# INLINE hashFingerprint #-}

mkFingerprint
    :: forall f a
    . KnownNat f
    => CuckooFilterHash a
    => Salt
    -> a
    -> Fingerprint f
mkFingerprint salt a = Fingerprint $! max 1 $!
    cuckooFingerprint salt a .&. (2 ^ w @f - 1)
{-# INLINE mkFingerprint #-}

bucket1 :: CuckooFilterHash a => CuckooFilter s b f a -> a -> Bucket
bucket1 f a = Bucket $! int $! cuckooHash (_cfSalt f) a .&. (int $ _cfBucketCount f - 1)
{-# INLINE bucket1 #-}

otherBucket :: CuckooFilter s b f a -> Bucket -> Fingerprint f -> Bucket
otherBucket f (Bucket b) fp = Bucket $!
    (b `xor` hashFingerprint (_cfSalt f) fp) .&. (int $ _cfBucketCount f - 1)
{-# INLINE otherBucket #-}

ix :: Bucket -> Int
ix (Bucket i) = i
{-# INLINE ix #-}

-- | Fingerprints must be of at most 32 bits. Yet we represent them as Word64,
-- this is compromise to work with an reasonably efficient 32bit alignment,
-- while guaranteeing that the for each fingerprint there is an alignment such
-- that the fingerprint fits into the returned value with respect to the
-- alignment.
--
readFingerprint
    :: forall m b f a
    . KnownNat f
    => KnownNat b
    => PrimMonad m
    => CuckooFilter (PrimState m) b f a
        -- ^ Filter
    -> Bucket
        -- ^ bucket number
    -> Slot
        -- ^ slot number
    -> m (Fingerprint f)
readFingerprint f n (Slot i) = do
    v <- get dat pos
    return $ Fingerprint $! (v `shiftR` off) .&. mask
  where
    dat = _cfData f
    mask = (2 ^ (w @f)) - 1
    (pos, off) = (w @b * w @f * ix n + w @f * i) `divMod` 32
{-# INLINE readFingerprint #-}

setFingerprint
    :: forall m b f a
    . KnownNat f
    => KnownNat b
    => PrimMonad m
    => CuckooFilter (PrimState m) b f a
        -- ^ Filter
    -> Bucket
        -- ^ bucket number
    -> Slot
        -- ^ slot number
    -> Fingerprint f
    -> m ()
setFingerprint f n (Slot i) (Fingerprint fp) = do
    v <- get dat pos
    set dat pos $ (fp `shiftL` off) .|. (v .&. complement mask)
  where
    dat = _cfData f
    mask = (2 ^ (w @f) - 1) `shiftL` off
    (pos, off) = (w @b * w @f * ix n + w @f * i) `divMod` 32
{-# INLINE setFingerprint #-}

-- -------------------------------------------------------------------------- --
-- Utils

null :: Fingerprint f
null = Fingerprint 0
{-# INLINE null #-}

getBucketsRandom
    :: CuckooFilterHash a
    => PrimMonad m
    => KnownNat f
    => KnownNat b
    => CuckooFilter (PrimState m) b f a
    -> a
    -> m (Bucket, Bucket, Fingerprint f)
getBucketsRandom f a = bool (b1, b2, fp) (b2, b1, fp) <$> MWC.uniform rng
  where
    salt = _cfSalt f
    rng = _cfRng f
    b1 = bucket1 f a
    b2 = otherBucket f b1 fp
    fp = mkFingerprint salt a
{-# INLINE getBucketsRandom #-}

checkBucket
    :: forall m b f a
    . PrimMonad m
    => KnownNat f
    => KnownNat b
    => CuckooFilter (PrimState m) b f a
    -> Bucket
    -> Fingerprint f
    -> m (Maybe Slot)
checkBucket f b fp = go (w @b - 1)
  where
    go :: Int -> m (Maybe Slot)
    go (-1) = return Nothing
    go i = readFingerprint f b (Slot i) >>= \x -> if x == fp
        then return $ Just (Slot i)
        else go (pred i)
    -- TODO: can we teach GHC to unroll this loop statically without
    -- defining a class. Maybe GHC does already unroll it?

-- | Total number of items that the filter can hold. In practice a load factor
-- of ~95% of this number can be reached.
--
capacityInItems :: forall s b f a . KnownNat b => CuckooFilter s b f a -> Int
capacityInItems f = _cfBucketCount f * w @b
{-# INLINE capacityInItems #-}

-- | The total number of bytes allocated for storing items in the filter.
--
sizeInAllocatedBytes :: forall s b f a . KnownNat f => KnownNat b => CuckooFilter s b f a -> Int
sizeInAllocatedBytes f = fit @Int @Int (capacityInItems f * w @f) 8
{-# INLINE sizeInAllocatedBytes #-}

-- | Number of items currently stored in the filter.
--
-- /Note/ that computing this number is expensive ( \(O(n)\) ).
--
itemCount
    :: forall m b f a
    . PrimMonad m
    => KnownNat b
    => KnownNat f
    => CuckooFilter (PrimState m) b f a
    -> m Int
itemCount f = foldM (\x i -> foldM (\x' j -> go x' (Bucket i) (Slot j)) x [0.. w @b - 1]) 0 [0.._cfBucketCount f - 1]
  where
    go x b s = readFingerprint f b s >>= \fp -> case fp == Fingerprint 0 of
        True -> return x
        False -> return (succ x)

-- | The current load factor of the filter in percent.
--
-- @
-- loadFactor f = 100 * itemCount f / capacityInItems
-- @
--
-- /Note/ that computing this number is expensive ( \(O(n)\) ).
--
loadFactor
    :: forall m b f a
    . PrimMonad m
    => KnownNat b
    => KnownNat f
    => CuckooFilter (PrimState m) b f a
    -> m Double
loadFactor f = do
    i <- itemCount f
    return $! 100 * int i / int (capacityInItems f)

-- -------------------------------------------------------------------------- --
-- Debugging Tools

-- | Show the contents of the filter as a list of buckets with values show in
-- hex. Used for debugging purposes.
--
showFilter
    :: forall b f a
    . KnownNat f
    => KnownNat b
    => CuckooFilter RealWorld b f a
    -> IO [[String]]
showFilter f = forM [0.. _cfBucketCount f - 1] $ \(i :: Int) -> do
        forM [0 .. w @b - 1] $ \(j :: Int) -> do
            Fingerprint fp <- readFingerprint f (Bucket i) (Slot j)
            return $ printf ("%0" <> show (fit @Int @Int @Int (w @f) 8) <> "x") fp

-- | Returns the different hashes that are associated with an item in the
-- filter. Used for debugging purposes.
--
itemHashes
    :: forall s b f a
    . KnownNat f
    => CuckooFilterHash a
    => CuckooFilter s b f a
    -> a
    -> (Int, Int, Word64)
itemHashes f a = (b1_, b2_, fp_)
  where
    fp@(Fingerprint fp_) = mkFingerprint @f (_cfSalt f) a
    b1@(Bucket b1_) = bucket1 f a
    Bucket b2_ = otherBucket f b1 fp
