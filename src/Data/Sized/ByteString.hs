{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Sized.ByteString
    ( SizedByteString(..)
    , NatReflection(..), NatProxy
    , fromByteString
    , toByteString
    , inlinePerformIO
    ) where

import GHC.Base (realWorld#)
import GHC.ForeignPtr (ForeignPtr, mallocPlainForeignPtrBytes)
import GHC.IO (IO(IO))

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString, unsafePackCStringLen)
import Data.Word (Word8)
import Foreign (Ptr, castPtr, withForeignPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as ByteString

import Data.Sized.Nat (Nat, NatProxy, NatReflection(nat))
import Data.Sized.Foreign (memcpy, memcmp)

data SizedByteString (size :: Nat) = SizedByteString {-# UNPACK #-} !(ForeignPtr Word8)
    deriving (Show)

instance NFData (SizedByteString size)

instance NatReflection a => Eq (SizedByteString a) where
    SizedByteString fp1 == SizedByteString fp2 = inlinePerformIO $ do
        withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> do
            cmp <- memcmp p1 p2 size
            return $ cmp == 0
      where
        size = fromIntegral $ nat (undefined :: NatProxy a)

instance NatReflection a => Ord (SizedByteString a) where
    SizedByteString fp1 `compare` SizedByteString fp2 = inlinePerformIO $ do
        withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> do
            memcmp p1 p2 size >>= \c -> return $ if
              | c == 0 -> EQ
              | c <  0 -> LT
              | c >  0 -> GT
      where
        size = fromIntegral $ nat (undefined :: NatProxy a)

instance NatReflection a => Storable (SizedByteString a) where
    sizeOf _ = nat (undefined :: NatProxy a)
    alignment _ = alignment (undefined :: Word8)
    peek = unsafeFromPtr
    poke ptr f@(SizedByteString fp) = withForeignPtr fp $ \p ->
        memcpy (castPtr ptr) p $ fromIntegral $ sizeOf f

unsafeFromPtr :: forall a b. NatReflection a => Ptr b -> IO (SizedByteString a)
unsafeFromPtr p = do
    let size = nat (undefined :: NatProxy a)
    fp <- mallocPlainForeignPtrBytes size
    withForeignPtr fp $ \p' -> do
        memcpy p' (castPtr p) $ fromIntegral size
        return $! SizedByteString fp
{-# INLINE unsafeFromPtr #-}

unsafeFromByteString :: forall a. NatReflection a => ByteString -> SizedByteString a
unsafeFromByteString b = unsafeCoerce $ unsafeDupablePerformIO $
    unsafeUseAsCString b $ \p -> unsafeFromPtr p :: IO (SizedByteString a)
{-# INLINE unsafeFromByteString #-}

fromByteString :: forall a. NatReflection a => ByteString -> Maybe (SizedByteString a)
fromByteString b
    | bsize == expected = Just $ unsafeFromByteString b
    | otherwise         = Nothing
  where
    bsize = ByteString.length b
    expected = nat (undefined :: NatProxy a)
{-# INLINE fromByteString #-}

toByteString :: forall a. NatReflection a => SizedByteString a -> ByteString
toByteString (SizedByteString fp) = unsafeDupablePerformIO $ withForeignPtr fp $ \p -> do
    let size = nat (undefined :: NatProxy a)
    unsafePackCStringLen (castPtr p, fromIntegral size)
{-# INLINE toByteString #-}

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
