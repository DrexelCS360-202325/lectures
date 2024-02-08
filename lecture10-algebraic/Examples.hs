{-# OPTIONS_GHC -fwarn-tabs #-}
-- Haskell is space sensitive

{-# OPTIONS_GHC -Wall -Wno-type-defaults -fno-warn-missing-signatures #-}
-- Turn on warnings

module Examples where

import Test.QuickCheck

--
-- Inductive definitions of natural numbers
--

data Nat = Zero | Succ Nat

-- isNat :: Nat -> Bool

nat2int :: Nat -> Int
nat2int = error "nat2int: not yet defined"

int2nat :: Int -> Nat
int2nat = error "int2nat: not yet defined"

addCheat :: Nat -> Nat -> Nat
addCheat m n = int2nat (nat2int m + nat2int n)

add :: Nat -> Nat -> Nat
add = error "add: not yet defined"

--
-- Inductive definitions of lists
--

data List a = Nil
            | Cons a (List a)
  deriving (Eq, Ord, Show)

mymap :: (a -> b) -> List a -> List b
mymap = error "mymap unimplemented"

prop_map_inc :: Bool
prop_map_inc = mymap (\x -> x + 1) (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) == (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))

myappend :: List a -> List a -> List a
myappend = error "myappend unimplemented"

prop_append :: Bool
prop_append = myappend xs ys == Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
  where
    xs = Cons 1 (Cons 2 Nil)
    ys = Cons 3 (Cons 4 Nil)

myreverse :: List a -> List a
myreverse = error "myreverse unimplemented"

prop_reverse :: Bool
prop_reverse = myreverse (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) == Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil)))

-- | This main function runs all tests
--
-- `main` is executed when you compile and run this program. We'll cover `do`
-- notation later in the course. You can add quickCheck tests here.
--
-- To run the tests, type `main` at the GHCi prompt.
main :: IO ()
main = do
    quickCheck prop_map_inc
    quickCheck prop_append
    quickCheck prop_reverse
