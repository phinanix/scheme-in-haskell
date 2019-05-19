{-# LANGUAGE FlexibleInstances #-}
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

anyOf :: String -> Parser Char
anyOf "" = empty
anyOf (x:xs) = char x <|> anyOf xs

validSymbol :: Char -> Bool
validSymbol c = elem c "+-*/!@$%^&_=|~"

validStart :: Char -> Bool
validStart c = or [isAlpha c, validSymbol c]

validRest :: Char -> Bool
validRest c = or [isAlphaNum c, validSymbol c]

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident =(:)<$> (satisfy validStart)
  <*> (zeroOrMore (satisfy validRest))

string :: Parser String
string =  zeroOrMore (satisfy validRest) 

integer :: Parser Integer
integer = (posInt) <|> (((-1) *) <$>  (char '-' *> posInt))

specificString :: String -> Parser String
specificString [x] = (:) <$> char x <*> (pure [])
specificString (x:xs) = (:) <$> char x <*> (specificString xs)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String
newtype Context = C { unwrap :: [(Ident, SExpr)]} deriving Show
type SchemeFunc = ([SExpr]->Either String SExpr)
instance Show SchemeFunc where
  show s = "function"
type MacroFunc  = ([SExpr]->Either String [SExpr])
instance Show MacroFunc where
  show s = "macro"
type SpecialFunc = (Context->[SExpr]->Either String SExpr)
instance Show SpecialFunc where
  show s = "special"

type Formals = [Ident]
type Body = SExpr
-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = N Integer | I Ident | S String 
           | BO Bool | EmptyList | F SchemeFunc 
           | Macro MacroFunc
           | Special SpecialFunc
           | Closure Context Formals Body
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser SExpr
parseAtom = (N <$> integer) <|> 
  (S <$> (char '"' *> string <* char '"')) <|>
  (char '#' *> char 't' *> pure (BO True)) <|>
  (char '#' *> char 'f' *> pure (BO False)) <|>
  (char '\'' *> char '(' *> char ')' *> pure EmptyList) <|> 
  (I <$> ident)


parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> 
  (Comb <$> (char '(' *> (oneOrMore parseSExpr) 
  <* spaces <* char ')'))) <* spaces