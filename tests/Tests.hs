module Main where

import Test.Framework (defaultMain)

import qualified Data.ByteString.Fixed.Instances.Tests

main :: IO ()
main = defaultMain
    [ Data.ByteString.Fixed.Instances.Tests.tests
    ]
