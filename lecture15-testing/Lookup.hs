{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Lookup where

import Test.Hspec

lookupTest :: Spec
lookupTest = it "lookup" $ lookup 1 [(1,"a")] `shouldBe` Just "b"
