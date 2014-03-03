module Data.Sized.ByteString.Tests (tests) where

import Data.Maybe (fromJust)
import Data.ByteString.Char8 (pack)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (Storable(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, Arbitrary(..), suchThat)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Data.Sized.ByteString (SizedByteString, fromByteString)

instance Arbitrary (SizedByteString 32) where
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

tests :: TestTree
tests = testGroup "Data.Sized.ByteString.Tests"
    [ testProperty "Storable instance for SizedByteString" (testStorable :: SizedByteString 32 -> Property)
    ]
