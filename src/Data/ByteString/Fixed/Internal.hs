{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Data.ByteString.Fixed.Internal
    ( FixedByteString(..)
    , NatReflection(..), NatProxy
    , fromByteString
    , toByteString
    , inlinePerformIO
    ) where

import GHC.Base (realWorld#)
import GHC.ForeignPtr (ForeignPtr, mallocPlainForeignPtrBytes)
import GHC.IO (IO(IO))
import GHC.TypeLits (Nat)

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import Data.Word (Word8)
import Foreign (castPtr, withForeignPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Data.ByteString.Fixed.Foreign (memcpy)

data FixedByteString (size :: Nat) = FixedByteString {-# UNPACK #-} !(ForeignPtr Word8)
    deriving (Show)

data NatProxy (nat :: Nat)

class NatReflection (nat :: Nat) where
    nat :: NatProxy nat -> Int

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

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
