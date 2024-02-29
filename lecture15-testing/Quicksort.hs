{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Quicksort where

import Test.Hspec
import Test.QuickCheck

qsort :: Ord a => [a] -> [a]
qsort []     =  []
qsort (x:xs) =  qsort (filter (< x) xs) ++
                [x] ++
                qsort (filter (> x) xs)

isSorted :: Ord a => [a] -> Bool
isSorted []  = True
isSorted [_] = True
isSorted (x:ys@(y:_))
  | x <= y    = isSorted ys
  | otherwise = False

prop_qsort1 :: [Int] -> Bool
prop_qsort1 l = isSorted $ qsort l

prop_qsort2 :: [Int] -> Bool
prop_qsort2 l = qsort l == qsort (reverse l)

prop_qsort3 :: [Int] -> [Int] -> Bool
prop_qsort3 l1 l2 = qsort (l1 ++ l2) == qsort (l2 ++ l1)

spec :: Spec
spec = do
  describe "Quicksort" $ do
    it "sort empty" $
      qsort [] `shouldBe` ([] :: [Int])
    it "sort sorted" $
      qsort [1,2,3] `shouldBe` ([1,2,3] :: [Int])
    it "sort unsorted" $
      qsort [3,2,1] `shouldBe` ([1,2,3] :: [Int])
