module Data.ByteString.Fixed.Instances.Tests (tests) where

import Data.Maybe (fromJust)
import Data.ByteString.Char8 (pack)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (Storable(..))

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, Arbitrary(..), suchThat)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Data.ByteString.Fixed (FixedByteString, fromByteString)

instance Arbitrary (FixedByteString 32) where
    arbitrary = do
        s <- arbitrary `suchThat` ((32 ==) . length)
        let b = pack s
        return $ fromJust $ fromByteString b

testStorable :: (Arbitrary a, Storable a, Eq a) => a -> Property
testStorable storable = monadicIO $ do
    peeked <- run $ do
        allocaBytes size $ \ptr -> do
            poke ptr storable
            peek ptr
    assert $ storable == peeked
  where
    size = sizeOf storable

tests :: Test
tests = testGroup "Data.ByteString.Fixed.Instances.Tests"
    [ testProperty "Storable instance for FixedByteString" (testStorable :: FixedByteString 32 -> Property)
    ]
