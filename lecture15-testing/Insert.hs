{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Insert where

import Test.Hspec

insert :: Ord a => a -> [a] -> [a]
insert = error "insert: not yet defined"

spec :: Spec
spec = do
  describe "insert" $ do
    it "insert item at start of list" $
      insert 1 [2,4] `shouldBe` ([1,2,4] :: [Integer])
    it "insert item in middle of list" $
      insert 3 [2,4] `shouldBe` ([2,3,4] :: [Integer])
    it "insert item at end of list" $
      insert 5 [2,4] `shouldBe` ([2,4,5] :: [Integer])
    it "insert item in empty list" $
      insert 1 [] `shouldBe` ([1] :: [Integer])
    it "insert item already in list" $
      insert 1 [1] `shouldBe` ([1,1] :: [Integer])
