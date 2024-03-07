{-# OPTIONS_GHC -Wall #-}

import Prelude hiding (lookup)

--
-- While language
--

type Z = Integer

type Var = String

data Aexp = Const Z | Var Var
          | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp
  deriving (Show)

data Bexp = BTrue | BFalse
          | Eq Aexp Aexp | Le Aexp Aexp
          | Not Bexp | And Bexp Bexp
  deriving (Show)

data Stm  = Assign Var Aexp | Skip | Seq Stm Stm
          | If Bexp Stm Stm
          | While Bexp Stm
  deriving (Show)

--
-- Functions for keeping track of program state
--

-- Fix this type definition!
type State = ()

empty :: State
empty = error "empty: not yet defined"

insert :: Var -> Z -> State -> State
insert = error "insert: not yet defined"

lookup :: Var -> State -> Z
lookup = error "lookup: not yet defined"

--
-- Big-step semantics for arithmetic expression
--

abig :: State -> Aexp -> Z
abig = error "abig: not yet defined"

--
-- Big-step semantics for Boolean expression
--

bbig :: State -> Bexp -> Bool
bbig = error "bbig: not yet defined"
