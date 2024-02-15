{-# OPTIONS_GHC -fwarn-tabs #-}
-- Haskell is space sensitive

{-# OPTIONS_GHC -Wall -Wno-type-defaults -fno-warn-missing-signatures #-}
-- Turn on warnings

--
-- You can start GHCi at the UNIX prompt with the command `stack ghci`.
--
-- You can load this file into GHCi by typing the command `:load Examples.hs` at
-- the GHCi prompt.

--
-- Partial and total functions
--

--
-- Define a safe head function
--

data MaybeHead = NoHead
               | JustHead Int

safeHeadInts = undefined

safeHead = undefined

--
-- Type classes
--

data Foo = F Int | G Char

-- instance Eq Foo where
