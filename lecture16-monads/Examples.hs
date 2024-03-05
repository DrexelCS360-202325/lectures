module Examples where

import Prelude hiding (Maybe(..), Either(..), map, mapM)

import System.Random

-- Defined as in Prelude
data Maybe a = Nothing
             | Just a
  deriving (Eq, Ord, Show, Read)

-- Defined as in Prelude
data Either a b = Left a
                | Right b
  deriving (Eq, Ord, Show, Read)

-- We can't hide the definition of the list data type, so we use an isomorphic
-- definition.
data List a = Nil
            | Cons a (List a)
  deriving (Eq, Ord, Show, Read)

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Read)

map :: (a -> b) -> List a -> List b
map _ Nil         = Nil
map f (Cons x xs) = Cons (f x ) (map f xs)

--
-- Functors
--
instance Functor List where
    -- fmap :: (a -> b) -> List a -> List b
    fmap = map

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap = undefined

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap = undefined

-- instance Functor Either... where

--
-- Applicatives
--

instance Applicative Maybe where
    -- pure :: a -> Maybe a
    pure = undefined

    -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    (<*>) = undefined

{-
instance Applicative [] where
    -- pure :: a -> [x]
    pure x = [x]

    -- (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs, x <- xs]
-}

--
-- Abstracting over computation
--

--
-- Total interpreter
--
data Exp = Const Int | Div Exp Exp

eval :: Exp -> Int
eval (Const n) = n
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval' :: Exp -> Maybe Int
eval' (Const n) = Just n
eval' (Div x y) = case eval' x of
                    Nothing -> Nothing
                    Just n  -> case eval' y of
                                 Nothing -> Nothing
                                 Just m  -> safediv n m

--
-- Total interpreter in monadic style
--

instance Monad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) = undefined

eval'' :: Exp -> Maybe Int
eval'' (Const n) = Just n
eval'' (Div x y) = eval'' x >>= \n ->
                   eval'' y >>= \m ->
                   safediv n m

--
-- Functions that use random numbers
--

newtype Randomized a = R (StdGen -> (a, StdGen))

runR :: Randomized a -> StdGen -> (a, StdGen)
runR (R f) = f

instance Functor Randomized where
    -- fmap :: (a -> b) -> R a -> R b
    fmap f g = R $ \s ->
        let (x, s') = runR g s
        in
          (f x, s')

instance Applicative Randomized where
    pure x = R $ \s -> (x, s)

    mf <*> mx = R $ \s -> let (f, s')  = runR mf s
                              (x, s'') = runR mx s'
                          in
                            (f x, s'')

instance Monad Randomized where
    mx >>= f = R $ \s -> let (y, s') = runR mx s
                         in
                           runR (f y) s'

--
-- State Monad
--

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Functor (State s) where
    fmap f m = undefined

instance Applicative (State s) where
    pure x = undefined

    mf <*> mx = undefined

instance Monad (State s) where
    mx >>= f = undefined

--
-- Generic functions
--

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM = undefined

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM = undefined
