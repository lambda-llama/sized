module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Data.Sized.ByteString.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Data.Sized.ByteString.Tests.tests
    ]
