-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.
-- Modified and extended by Geoffrey Mainland

module Parsing (
   module Parsing,
   module Control.Applicative
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P $ \inp -> case inp of
                     []   -> []
                     x:xs -> [(x,xs)]

eof :: Parser ()
eof = P $ \inp -> case inp of
                    [] -> [((), [])]
                    _  -> []

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P $ \inp -> [(g v, out) | (v, out) <- parse p inp]

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P $ \inp -> [(v,inp)]

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P $ \inp -> concat [parse (fmap g px) out | (g,out) <- parse pg inp]

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P $ \inp -> concat [parse (f v) out | (v,out) <- parse p inp]

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P $ \_ -> []

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P $ \inp -> case parse p inp of
                           [] -> parse q inp
                           vs -> vs

instance MonadFail Parser where
    fail _ = P $ \_ -> []

instance MonadPlus Parser where
    mzero       = fail "mzero"
    m `mplus` n = P $ \inp -> case parse m inp of
                                [] -> parse n inp
                                vs -> vs

-- Derived primitives

try :: Parser a -> Parser ()
try p = (p >> return ()) <|> return ()

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

oneOf :: [Char] -> Parser Char
oneOf cs = sat (`elem` cs)

noneOf :: [Char] -> Parser Char
noneOf cs = sat (`notElem` cs)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do _ <- char x
                   _ <- string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Integer
nat = do xs <- some digit
         return (read xs)

int :: Parser Integer
int = do _ <- char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do _ <- many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

keyword :: String -> Parser ()
keyword xs = do _ <- token (string xs)
                return ()

identifier :: Parser String
identifier = token ident

natural :: Parser Integer
natural = token nat

integer :: Parser Integer
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

surroundedBy :: Parser a
             -> Parser b
             -> Parser c
             -> Parser c
surroundedBy open close parser = do
    _ <- open
    x <- parser
    _ <- close
    return x

parens :: Parser a -> Parser a
parens = surroundedBy (char '(') (char ')')

-- | @chainl p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@, and returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. If there are zero occurrences of @p@, the value @x@ is returned.
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op x = chainl1 p op <|> return x

-- | @chainl1 p op x@ parses /one/ or more occurrences of @p@,
-- separated by @op@, and returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. This parser can, for example, be used to eliminate left recursion which
-- typically occurs in expression grammars.
--
-- >  expr    = term   `chainl1` addop
-- >  term    = factor `chainl1` mulop
-- >  factor  = parens expr <|> integer
-- >
-- >  mulop   =   do{ symbol "*"; return (*)   }
-- >          <|> do{ symbol "/"; return (div) }
-- >
-- >  addop   =   do{ symbol "+"; return (+) }
-- >          <|> do{ symbol "-"; return (-) }
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do x <- p
                  rest x
  where
    rest x =  do f <- op
                 y <- p
                 rest (f x y)
          <|> return x

-- | @chainr p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@, and returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. If there are no occurrences of @p@, the value @x@ is returned.
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op x = chainr1 p op <|> return x

-- | @chainr1 p op x@ parses /one/ or more occurrences of |p|,
-- separated by @op@, and returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned by @p@.
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op =
    scan
  where
    scan = do x <- p
              rest x

    rest x =  do f <- op
                 y <- scan
                 return (f x y)
          <|> return x
