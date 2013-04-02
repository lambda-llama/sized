{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds #-}

module Data.ByteString.Fixed.Internal
    ( FixedByteString(..)
    , fromByteString
    , toByteString
    ) where

import GHC.ForeignPtr (ForeignPtr, mallocPlainForeignPtrBytes)
import GHC.TypeLits (Nat)

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import Data.Word (Word8)
import Foreign (castPtr, withForeignPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Data.ByteString.Fixed.Internal.Foreign (memcpy, memcmp)

data FixedByteString (size :: Nat) = FixedByteString {-# UNPACK #-} !(ForeignPtr Word8)
    deriving (Show)

data NatProxy (nat :: Nat)

class NatReflection (nat :: Nat) where
    nat :: NatProxy nat -> Int

instance NFData (FixedByteString size)

instance NatReflection a => Eq (FixedByteString a) where
    FixedByteString fp1 == FixedByteString fp2 = unsafeDupablePerformIO $ do
        withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> do
            cmp <- memcmp p1 p2 size
            return $ cmp == 0
      where
        size = fromIntegral $ nat (undefined :: NatProxy a)

instance NatReflection a => Ord (FixedByteString a) where
    FixedByteString fp1 `compare` FixedByteString fp2 = unsafeDupablePerformIO $ do
        withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> do
            memcmp p1 p2 size >>= \c -> return $ if
              | c == 0 -> EQ
              | c <  0 -> LT
              | c >  0 -> GT
      where
        size = fromIntegral $ nat (undefined :: NatProxy a)

fromByteString :: forall a. NatReflection a => ByteString -> Maybe (FixedByteString a)
fromByteString b = unsafeCoerce $ unsafeDupablePerformIO $
    unsafeUseAsCStringLen b $ \(p, size) -> do
        let expectedSize = nat (undefined :: NatProxy a)
        case expectedSize == size of
            True -> do
                fp <- mallocPlainForeignPtrBytes size
                withForeignPtr fp $ \p' -> do
                    memcpy p' (castPtr p) $ fromIntegral size
                    return $! Just $ FixedByteString fp
            False -> return $ Nothing
{-# INLINE fromByteString #-}

toByteString :: forall a. NatReflection a => FixedByteString a -> ByteString
toByteString (FixedByteString fp) = unsafeDupablePerformIO $ withForeignPtr fp $ \p -> do
    let size = nat (undefined :: NatProxy a)
    unsafePackCStringLen (castPtr p, fromIntegral size)
{-# INLINE toByteString #-}
