module Data.ByteString.Fixed
    ( FixedByteString(..)
    , NatReflection(..), NatProxy
    , fromByteString
    , toByteString
    ) where

import Data.ByteString.Fixed.Internal (FixedByteString(..), NatReflection(..),
                                       NatProxy, fromByteString, toByteString)
import Data.ByteString.Fixed.Instances ()
