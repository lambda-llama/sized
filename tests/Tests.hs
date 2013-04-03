module Main where

import Test.Framework (defaultMain)

import qualified Data.Sized.ByteString.Tests

main :: IO ()
main = defaultMain
    [ Data.Sized.ByteString.Tests.tests
    ]
