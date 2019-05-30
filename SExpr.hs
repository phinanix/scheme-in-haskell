{-# LANGUAGE FlexibleInstances #-}
{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char
import Data.Semigroup

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
validSymbol c = elem c "+-*/!@$%^&_=|~\'"

validStart :: Char -> Bool
validStart c = or [isAlpha c, validSymbol c]

validRest :: Char -> Bool
validRest c = or [isAlphaNum c, validSymbol c]

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace <|> char '\n')

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
instance Semigroup Context where
  --inner idents shadow outer idents
  (C outer) <> (C inner) = C (inner ++ outer)
instance Monoid Context where
  mempty = C []
  mappend = (<>)

type SchemeFunc = ([SExpr]->Either String SExpr)
type MacroFunc  = ([SExpr]->Either String [SExpr])
type SpecialFunc = (Context,[SExpr])->Either String (Context,SExpr)

type Formals = [Ident]
type Body = SExpr
-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = N Integer | I Ident | S String 
           | BO Bool | EmptyList | F SchemeFunc 
           | Macro MacroFunc
           | Special SpecialFunc
           | Closure Context Formals Body
           | Comb [SExpr]
instance Show SExpr where
  show (N integer)     = show integer
  show (I ident)       = "'" ++ ident
  show (S string)      = show string
  show (BO bool)       = show bool
  show EmptyList       = "'()"
  show (F _)           = "builtin"
  show (Macro _)       = "macro"
  show (Special _)     = "special"
  show (Closure c f b) = "(lambda " ++ show f ++ show b ++ ")"
  show (Comb l)        = "(" ++ unwords (fmap show l) ++ ")"
  

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