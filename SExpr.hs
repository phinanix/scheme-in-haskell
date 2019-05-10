{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> (pure [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident =(:)<$> (satisfy isAlpha) <*> (zeroOrMore (satisfy isAlphaNum))

string :: Parser String
string =  zeroOrMore (satisfy isAlphaNum) 

integer :: Parser Integer
integer = (posInt) <|> (((-1) *) <$>  (char '-' *> posInt))

specificString :: String -> Parser String
specificString [x] = (:) <$> char x <*> (pure [])
specificString (x:xs) = (:) <$> char x <*> (specificString xs)

builtin :: Parser Builtin
builtin = (char '+' *> (pure Plus)) <|>
          (char '-' *> (pure Minus)) <|>
          (char '*' *> (pure Times)) <|>
          (char '/' *> (pure Divide)) <|>
          (specificString "if") *> (pure If) <|>
          (specificString "and") *> (pure And) <|>
          (specificString "or") *> (pure Or) <|>
          (specificString "cons") *> (pure Cons) <|>
          (specificString "car") *> (pure Car) <|>
          (specificString "cdr") *> (pure Cdr) <|>
          (specificString "quote") *> (pure Quote) 
------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

data Builtin = Plus | Minus | Times | Divide 
             | If | Cons | Car | Cdr | Quote
             | And | Or

  deriving (Show, Eq)

-- An "atom" is an integer value, an identifier, a string, or a builtin function
data Atom = N Integer | I Ident | S String | BI Builtin 
          | BO Bool | Null
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = (N <$> integer) <|> 
  (BI <$> builtin) <|> (S <$> (char '"' *> string <* char '"')) <|>
  (char '#' *> char 't' *> pure (BO True)) <|>
  (char '#' *> char 'f' *> pure (BO False)) <|>
  (char '\'' *> char '(' *> char ')' *> pure Null) <|> (I <$> ident)


parseSExpr :: Parser SExpr
parseSExpr = spaces *> ((A <$> parseAtom) <|> 
  (Comb <$> (char '(' *> (oneOrMore parseSExpr) 
  <* spaces <* char ')'))) <* spaces