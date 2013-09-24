{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Sized.ByteString
    ( SizedByteString(..)
    , NatReflection(..), Proxy
    , fromByteString
    , toByteString
    , length
    ) where

import GHC.Base (realWorld#)
import GHC.ForeignPtr (ForeignPtr, mallocPlainForeignPtrBytes)
import GHC.IO (IO(IO))
import Prelude hiding (length)

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString, unsafePackCStringLen)
import Data.Word (Word8)
import Foreign (Ptr, castPtr, withForeignPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as ByteString

import Data.Proxy (Proxy(..))

import Data.Sized.Nat (Nat, NatReflection(nat))
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
        size = fromIntegral $ nat (Proxy :: Proxy a)

instance NatReflection a => Ord (SizedByteString a) where
    SizedByteString fp1 `compare` SizedByteString fp2 = inlinePerformIO $ do
        withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> do
            memcmp p1 p2 size >>= \c -> return $ if
                | c == 0 -> EQ
                | c <  0 -> LT
                | c >  0 -> GT
      where
        size = fromIntegral $ nat (Proxy :: Proxy a)

instance NatReflection a => Storable (SizedByteString a) where
    sizeOf _ = nat (Proxy :: Proxy a)
    alignment _ = alignment (undefined :: Word8)
    peek = unsafeFromPtr
    poke ptr f@(SizedByteString fp) = withForeignPtr fp $ \p ->
        memcpy (castPtr ptr) p $ fromIntegral $ sizeOf f

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

unsafeFromPtr :: forall a b. NatReflection a => Ptr b -> IO (SizedByteString a)
unsafeFromPtr p = do
    let size = nat (Proxy :: Proxy a)
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
fromByteString b = let bsize = ByteString.length b
                       expected = nat (Proxy :: Proxy a) in if
    | bsize == expected -> Just $ unsafeFromByteString b
    | otherwise         -> Nothing
{-# INLINE fromByteString #-}

toByteString :: forall a. NatReflection a => SizedByteString a -> ByteString
toByteString (SizedByteString fp) = unsafeDupablePerformIO $ withForeignPtr fp $ \p -> do
    let size = nat (Proxy :: Proxy a)
    unsafePackCStringLen (castPtr p, fromIntegral size)
{-# INLINE toByteString #-}

length :: forall a. NatReflection a => SizedByteString a -> Int
length _ = nat (Proxy :: Proxy a)
{-# INLINE length #-}
