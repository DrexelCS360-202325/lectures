module Exp where

import Parsing

data Exp = Lit Integer
         | Add Exp Exp
         | Mul Exp Exp
  deriving (Eq, Ord, Read, Show)

expr :: Parser Exp
expr = chainl1 term add

term :: Parser Exp
term = chainl1 factor mul

factor :: Parser Exp
factor = parens expr <|> lit

lit :: Parser Exp
lit = do { i <- integer
         ; return $ Lit i
         }

add :: Parser (Exp -> Exp -> Exp)
add = do { _ <- symbol "+"
         ; return Add
         }

mul :: Parser (Exp -> Exp -> Exp)
mul = do { _ <- symbol "*"
         ; return Mul
         }
