

module SEval where

import Control.Monad

import AParser
import SExpr

--the Left String represents a string error, else the function returns the value given on the right

biPlus :: [SExpr] -> Either String SExpr               
biPlus = foldr aggregate (Right $ N $ 0) where
    aggregate (N n) (Right (N acc)) = Right $ N $ n + acc
    aggregate _ (Left err) = Left err
    aggregate notnum _ = Left $ (show notnum) ++ "is not a number"
--minus :: int -> int -> int
--liftMinus :: int -> Either String SExpr -> Either String SExpr
biMinus :: [SExpr] -> Either String SExpr
biMinus ((N n):rest) = nMinus =<< biPlus rest
    where nMinus (N m) = Right $ N $ n - m
          nMinus notNum = Left $ (show notNum) ++ " is not an integer"
biMinus list = Left $ (show list) ++
             " is not a valid argument to -"

biTimes :: [SExpr] -> Either String SExpr
biTimes = foldr aggregate (Right $ N $ 1) where
    aggregate (N n) (Right (N acc)) = Right $ N $ n * acc
    aggregate _ (Left err) = Left err
    aggreate notnum _ = Left $ (show notnum) ++ "is not a number"

{- todo: floats don't exist yet
biDivide :: [SExpr] -> Either String SExpr
biDivide ((Nval n):rest) = foldr aggregate 
                                    (Right $ Nval $ n) rest 
    where
    aggregate (Nval n) (Right (Nval acc)) = Right $ Nval $ n / acc
    aggregate _ (Left err) = Left err
    aggreate notnum _ = Left $ (show notnum) ++ "is not a number"
biDivide notnums = Left $ (show notnums) ++ "do not begin with a number"-}

biIf :: [SExpr] -> Either String SExpr
biIf l@[x,y,z] = evalFirst l >>= listIf
    where listIf ((BO True),_)  = eval y
          listIf ((BO False),_) = eval z
          listIf (x,_) = Left $ (show x) ++ " is not a boolean"
biIf wronglen = Left $ (show wronglen) ++ " is the wrong length for an if statement"

biCons :: [SExpr] -> Either String SExpr
biCons [x, y] = Right $ Comb [x, y]
biCons nottwo = Left $ "cons was applied to the wrong number of arguments:" ++ (show nottwo)
biCar :: [SExpr] -> Either String SExpr
biCar [x] = eval x >>= unwrap where 
    unwrap (Comb (s1:_)) = Right s1
    unwrap notpair = Left $ (show notpair) ++ " is not a list"
biCar toomany = Left $ "car was applied to too many arguments" ++ (show toomany)
biCdr :: [SExpr] -> Either String SExpr
biCdr [x] = eval x >>= unwrap where 
    unwrap (Comb (s1:s2)) = Right $ Comb s2
    unwrap notpair = Left $ (show notpair) ++ " is not a list"
biCdr toomany = Left $ "cdr was applied to too many arguments" ++ (show toomany)
biAnd :: [SExpr] -> Either String SExpr
biAnd [] = Right $ BO True
biAnd [x] = eval x
biAnd (x:xs) = (eval x) >>= continue 
    where continue f@(BO False) = Right f
          continue truthy = biAnd xs
biOr :: [SExpr] -> Either String SExpr
biOr [] = Right $ BO False
biOr (x:xs) = (eval x) >>= continue 
    where continue f@(BO False) = biOr xs
          continue truthy = Right truthy

biQuote :: [SExpr] -> Either String SExpr
biQuote [] = Left "Quote called on too few arguments"
biQuote l = Right $ Comb l

{-to write quote, it seems like we need some sort of intermediate 
representation, it's not a SchemeVal because it's not fully evaluated, but it's not an SExpr either . . . is it?
like scheme is weakly typed, the type of quote is, idk
-}
{- like is what's supposed to happen we parse the input into a series of nested lists and then we repeatedly call evaluate on all of those lists until we get something?
where `parseSExpr` makes the nested lists and then eval takes care of evaluating them?
-}
builtInsList :: [(Ident, SExpr)]
builtInsList = [("+", F biPlus), 
                ("-", F biMinus), 
                ("*", F biTimes),
                ("if", Special biIf),
                ("cons", Special biCons),
                ("car", Special biCar),
                ("cdr", Special biCdr),
                ("and", Special biAnd),
                ("or", Special biOr),
                ("quote", Special biQuote)]

{- eval needs to know the difference between a function, which
takes a list of evaluated arguments and evaluates them, and a 
"special form", which does something to the code-tree and then 
returns the rest of the code tree-}     
eval :: SExpr -> Either String SExpr
eval val@(N n) = Right val
eval val@(S str) = Right val
eval val@(BO bool) = Right val
eval EmptyList = Right EmptyList
eval (I token) = wrap $ lookup token builtInsList where
    wrap Nothing = Left $ (show token) ++ "not found"
    wrap (Just func) = Right func
eval (Comb (x:xs)) =  ((:) <$> eval x <*> pure xs) >>= combEval
eval somethingelse = Left $ "got " ++ (show somethingelse)

combEval :: [SExpr] -> Either String SExpr
combEval ((F func):rest) = (mapM eval rest) >>= func
combEval ((Macro m):rest) = (m rest) >>= (eval . Comb)
combEval ((Special s):rest) = s rest
combEval (x:rest) = Left $ "Tried to call " ++ (show x) ++ 
    " (not a function) on " ++ (show rest)

evaluateScheme :: String -> Either String SExpr
evaluateScheme = ((=<<) eval) . unpack . (runParser parseSExpr) 
    where unpack Nothing = Left "Parsing failed"
          unpack (Just (sexpr, "")) = Right sexpr
          unpack (Just (_, leftover)) = Left $ "parsed but had " ++ 
            leftover ++ "remaining at end"