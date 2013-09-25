{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

module Main (main) where

import Prelude hiding (replicate)
import GHC.TypeLits

import Data.ByteString.Char8 (ByteString, replicate, snoc)
import Data.Maybe (fromJust)
import qualified Data.ByteString as ByteString

import Criterion.Main (defaultMain, bench, whnf)

import Data.Sized.ByteString (SizedByteString, fromByteString)
import qualified Data.Sized.ByteString as SizedByteString

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 707)
type instance 31 + 1 = 32
instance 1 <= 31
#endif

main :: IO ()
main = do
    let bs1 :: ByteString = replicate 31 'a'
    let bs2 :: ByteString = snoc (replicate 30 'a') 'b'
    let fbs1 :: SizedByteString 31 = fromJust $ fromByteString bs1
    let fbs2 :: SizedByteString 31 = fromJust $ fromByteString bs2
    defaultMain [ bench "eq bytestring" $ whnf ( == bs1) bs2
                , bench "eq fixed" $ whnf ( == fbs1) fbs2
                , bench "ord bytestring" $ whnf (`compare` bs1) bs2
                , bench "ord fixed" $ whnf (`compare` fbs1) fbs2
                , bench "singleton bytestring" $ whnf (ByteString.singleton) 0
                , bench "singleton fixed" $ whnf (SizedByteString.singleton) 0
                , bench "null bytestring" $ whnf (ByteString.null) bs1
                , bench "null fixed" $ whnf (SizedByteString.null) fbs1
                , bench "cons bytestring" $ whnf (ByteString.cons 0) bs1
                , bench "cons fixed" $ whnf (SizedByteString.cons 0) fbs1
                , bench "snoc bytestring" $ whnf (ByteString.snoc bs1) 32
                , bench "snoc fixed" $ whnf (SizedByteString.snoc fbs1) 32
                , bench "head bytestring" $ whnf (ByteString.head) bs1
                , bench "head fixed" $ whnf (SizedByteString.head) fbs1
                , bench "last bytestring" $ whnf (ByteString.last) bs1
                , bench "last fixed" $ whnf (SizedByteString.last) fbs1
                ]
