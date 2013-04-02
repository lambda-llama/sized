{-# LANGUAGE ForeignFunctionInterface #-}

module Data.ByteString.Fixed.Foreign
    ( memcpy
    , memcmp
    ) where

import Data.Word (Word8)
import Foreign (Ptr)
import Foreign.C.Types (CSize(..), CInt(..))

foreign import ccall unsafe "string.h memcpy" memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
foreign import ccall unsafe "string.h memcmp" memcmp :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt
