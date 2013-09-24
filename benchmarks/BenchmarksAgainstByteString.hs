module Main (main) where

import Prelude hiding (replicate)
import Data.ByteString.Char8 (ByteString, replicate, snoc)
import Data.Maybe (fromJust)
import qualified Data.ByteString as ByteString

import Criterion.Main (defaultMain, bench, whnf)

import Data.Sized.ByteString (SizedByteString, fromByteString)
import qualified Data.Sized.ByteString as SizedByteString

main :: IO ()
main = do
    let bs1 :: ByteString = replicate 32 'a'
    let bs2 :: ByteString = snoc (replicate 31 'a') 'b'
    let fbs1 :: SizedByteString 32 = fromJust $ fromByteString bs1
    let fbs2 :: SizedByteString 32 = fromJust $ fromByteString bs2
    defaultMain [ bench "eq bytestring" $ whnf ( == bs1) bs2
                , bench "eq fixed" $ whnf ( == fbs1) fbs2
                , bench "ord bytestring" $ whnf (`compare` bs1) bs2
                , bench "ord fixed" $ whnf (`compare` fbs1) fbs2
                , bench "singleton bytestring" $ whnf (ByteString.singleton) 0
                , bench "singleton fixed" $ whnf (SizedByteString.singleton) 0
                ]
