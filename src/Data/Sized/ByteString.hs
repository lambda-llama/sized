{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Sized.ByteString
    ( SizedByteString(..)
    , NatReflection(..), Proxy

    , fromByteString
    , toByteString

    , empty
    , singleton
    , null
    , length
    , cons
    ) where

import GHC.Base (realWorld#)
import GHC.ForeignPtr (ForeignPtr, mallocPlainForeignPtrBytes)
import GHC.IO (IO(IO))
import GHC.TypeLits
import Prelude hiding (length, null)

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString, unsafePackCStringLen)
import Data.Word (Word8)
import Foreign (Ptr, castPtr, plusPtr, withForeignPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as ByteString

import Data.Proxy (Proxy(..))

import Data.Sized.Nat (NatReflection(nat))
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

unsafeCreate :: forall a. NatReflection a => (Ptr Word8 -> IO ()) -> IO (SizedByteString a)
unsafeCreate f = do
    let size = nat (Proxy :: Proxy a)
    fp <- mallocPlainForeignPtrBytes size
    withForeignPtr fp f
    return $! SizedByteString fp
{-# INLINE unsafeCreate #-}

unsafeFromPtr :: forall a b. NatReflection a => Ptr b -> IO (SizedByteString a)
unsafeFromPtr p = do
    let size = nat (Proxy :: Proxy a)
    unsafeCreate $ \p' -> memcpy p' (castPtr p) $ fromIntegral size
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

empty :: SizedByteString 0
empty = SizedByteString $ unsafeDupablePerformIO $ mallocPlainForeignPtrBytes 0
{-# NOINLINE empty #-}

singleton :: Word8 -> SizedByteString 1
singleton c = unsafeDupablePerformIO $ unsafeCreate $ \p -> poke p c
{-# INLINE singleton #-}

null :: forall a. NatReflection a => SizedByteString a -> Bool
null = const $ nat (Proxy :: Proxy a) == 0
{-# INLINE null #-}

length :: forall a. NatReflection a => SizedByteString a -> Int
length = const $ nat (Proxy :: Proxy a)
{-# INLINE length #-}

cons :: forall a b. (b ~ (a + 1), NatReflection a, NatReflection b) => Word8 -> SizedByteString a -> SizedByteString b
cons = \c b@(SizedByteString fps) -> unsafeDupablePerformIO $ unsafeCreate $ \pd -> do
    poke pd c
    withForeignPtr fps $ \ps -> memcpy (pd `plusPtr` 1) ps $ fromIntegral $ length b
{-# INLINE cons #-}
