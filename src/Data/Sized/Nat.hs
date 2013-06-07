module Data.Sized.Nat
    ( Nat
    , NatReflection(..)
    ) where

import GHC.TypeLits (Nat)

import Data.Proxy (Proxy(..))

class NatReflection (nat :: Nat) where
    nat :: Proxy nat -> Int

instance NatReflection 0 where
    nat Proxy = 0

instance NatReflection 1 where
    nat Proxy = 1

instance NatReflection 2 where
    nat Proxy = 2

instance NatReflection 3 where
    nat Proxy = 3

instance NatReflection 4 where
    nat Proxy = 4

instance NatReflection 5 where
    nat Proxy = 5

instance NatReflection 6 where
    nat Proxy = 6

instance NatReflection 7 where
    nat Proxy = 7

instance NatReflection 8 where
    nat Proxy = 8

instance NatReflection 9 where
    nat Proxy = 9

instance NatReflection 10 where
    nat Proxy = 10

instance NatReflection 11 where
    nat Proxy = 11

instance NatReflection 12 where
    nat Proxy = 12

instance NatReflection 13 where
    nat Proxy = 13

instance NatReflection 14 where
    nat Proxy = 14

instance NatReflection 15 where
    nat Proxy = 15

instance NatReflection 16 where
    nat Proxy = 16

instance NatReflection 17 where
    nat Proxy = 17

instance NatReflection 18 where
    nat Proxy = 18

instance NatReflection 19 where
    nat Proxy = 19

instance NatReflection 20 where
    nat Proxy = 20

instance NatReflection 21 where
    nat Proxy = 21

instance NatReflection 22 where
    nat Proxy = 22

instance NatReflection 23 where
    nat Proxy = 23

instance NatReflection 24 where
    nat Proxy = 24

instance NatReflection 25 where
    nat Proxy = 25

instance NatReflection 26 where
    nat Proxy = 26

instance NatReflection 27 where
    nat Proxy = 27

instance NatReflection 28 where
    nat Proxy = 28

instance NatReflection 29 where
    nat Proxy = 29

instance NatReflection 30 where
    nat Proxy = 30

instance NatReflection 31 where
    nat Proxy = 31

instance NatReflection 32 where
    nat Proxy = 32
