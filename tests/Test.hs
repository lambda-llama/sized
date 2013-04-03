{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Control.Concurrent
import Control.Monad
import System.Mem
import Data.ByteString.Fixed

getFixed :: IO (FixedByteString 3)
getFixed = return $ fromJust $ fromByteString "foo"

getBs :: FixedByteString 3 -> IO ByteString
getBs = return . toByteString

main :: IO ()
main = do
    fbs1 <- getFixed
    bs2 <- getBs fbs1
    replicateM_ 1000 $ do
        print bs2
        performGC
        threadDelay 1000000
