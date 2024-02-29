{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Fib where

import Test.Hspec
import Test.QuickCheck

badfib :: Integer -> Integer
badfib n0 = aux n0 0 1
  where
    aux 0 _ b = b
    aux n a b = aux (n - 1) b (a + b)

fib :: Integer -> Integer
fib n0 = aux n0 0 1
  where
    aux 0 a _ = a
    aux n a b = aux (n - 1) b (a + b)

prop_fib0 :: Bool
prop_fib0 = fib 0 == 0

prop_fib1 :: Bool
prop_fib1 = fib 1 == 1

prop_fibn :: Integer -> Bool
prop_fibn n = fib n == fib (n-1) + fib (n-2)

spec :: Spec
spec = do
  describe "Fibonacci" $ do
    it "fib 0 = 0" $
      property prop_fib0
    it "fib 1 = 1" $
      property prop_fib1
    it "fib n = fib (n-1) + fib (n-2)" $
      property prop_fibn
