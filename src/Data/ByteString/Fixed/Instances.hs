{-# LANGUAGE MultiWayIf #-}

module Data.ByteString.Fixed.Instances () where

import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import Control.DeepSeq (NFData)
import Data.Word (Word8)
import Foreign (withForeignPtr, castPtr)
import Foreign.Storable (Storable(..))

import Data.ByteString.Fixed.Foreign (memcmp, memcpy)
import Data.ByteString.Fixed.Internal (FixedByteString(..), NatReflection(..), NatProxy,
                                       inlinePerformIO)

instance NFData (FixedByteString size)

instance NatReflection a => Eq (FixedByteString a) where
    FixedByteString fp1 == FixedByteString fp2 = inlinePerformIO $ do
        withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> do
            cmp <- memcmp p1 p2 size
            return $ cmp == 0
      where
        size = fromIntegral $ nat (undefined :: NatProxy a)

instance NatReflection a => Ord (FixedByteString a) where
    FixedByteString fp1 `compare` FixedByteString fp2 = inlinePerformIO $ do
        withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> do
            memcmp p1 p2 size >>= \c -> return $ if
              | c == 0 -> EQ
              | c <  0 -> LT
              | c >  0 -> GT
      where
        size = fromIntegral $ nat (undefined :: NatProxy a)

instance NatReflection a => Storable (FixedByteString a) where
    sizeOf _ = nat (undefined :: NatProxy a)
    alignment _ = alignment (undefined :: Word8)
    peek ptr = do
        let size = nat (undefined :: NatProxy a)
        fp <- mallocPlainForeignPtrBytes size
        withForeignPtr fp $ \p' -> do
            memcpy p' (castPtr ptr) $ fromIntegral size
            return $ FixedByteString fp
    poke ptr f@(FixedByteString fp) = withForeignPtr fp $ \p ->
        memcpy (castPtr ptr) p $ fromIntegral $ sizeOf f

instance NatReflection 0 where
    nat _ = 0

instance NatReflection 1 where
    nat _ = 1

instance NatReflection 2 where
    nat _ = 2

instance NatReflection 3 where
    nat _ = 3

instance NatReflection 4 where
    nat _ = 4

instance NatReflection 5 where
    nat _ = 5

instance NatReflection 6 where
    nat _ = 6

instance NatReflection 7 where
    nat _ = 7

instance NatReflection 8 where
    nat _ = 8

instance NatReflection 9 where
    nat _ = 9

instance NatReflection 10 where
    nat _ = 10

instance NatReflection 11 where
    nat _ = 11

instance NatReflection 12 where
    nat _ = 12

instance NatReflection 13 where
    nat _ = 13

instance NatReflection 14 where
    nat _ = 14

instance NatReflection 15 where
    nat _ = 15

instance NatReflection 16 where
    nat _ = 16

instance NatReflection 17 where
    nat _ = 17

instance NatReflection 18 where
    nat _ = 18

instance NatReflection 19 where
    nat _ = 19

instance NatReflection 20 where
    nat _ = 20

instance NatReflection 21 where
    nat _ = 21

instance NatReflection 22 where
    nat _ = 22

instance NatReflection 23 where
    nat _ = 23

instance NatReflection 24 where
    nat _ = 24

instance NatReflection 25 where
    nat _ = 25

instance NatReflection 26 where
    nat _ = 26

instance NatReflection 27 where
    nat _ = 27

instance NatReflection 28 where
    nat _ = 28

instance NatReflection 29 where
    nat _ = 29

instance NatReflection 30 where
    nat _ = 30

instance NatReflection 31 where
    nat _ = 31

instance NatReflection 32 where
    nat _ = 32
