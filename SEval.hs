
module SEval where

import Control.Monad

import AParser
import SExpr

--the Left String represents a string error, else the function returns the value given on the right
type SchemeFunc = ([SchemeVal]->Either String SchemeVal)

data SchemeVal = Nval Integer 
               | Sval String
               | Operator SchemeFunc
               | Pair SchemeVal SchemeVal
               | Boolean Bool
               | EmptyList
               | Macro ([SExpr]-> Either String [SExpr])
               | Special ([SExpr]-> Either String SchemeVal)

instance Show SchemeVal where 
    show (Nval n) = show n
    show (Sval s) = show s
    show (Operator _) = "operator"
    show (Pair p1 p2) = "(" ++ show p1 ++" "++ show p2 ++ ")"
    show (Boolean True) = "#t"
    show (Boolean False) = "#f"
    show EmptyList = "'()"is194/hw11 $ cloc .
    2 text files.

    show (Macro _) = "macro"
    show (Special _) = "special form"

biPlus :: [SchemeVal] -> Either String SchemeVal               
biPlus = foldr aggregate (Right $ Nval $ 0) where
    aggregate (Nval n) (Right (Nval acc)) = Right $ Nval $ n + acc
    aggregate _ (Left err) = Left err
    aggregate notnum _ = Left $ (show notnum) ++ "is not a number"
--minus :: int -> int -> int
--liftMinus :: int -> Either String SchemeVal -> Either String Schemeval
biMinus :: [SchemeVal] -> Either String SchemeVal
biMinus ((Nval n):rest) = nMinus =<< biPlus rest
    where nMinus (Nval m) = Right $ Nval $ n - m
          nMinus notNum = Left $ (show notNum) ++ " is not an integer"
biMinus list = Left $ (show list) ++
             " is not a valid argument to -"

biTimes :: [SchemeVal] -> Either String SchemeVal
biTimes = foldr aggregate (Right $ Nval $ 1) where
    aggregate (Nval n) (Right (Nval acc)) = Right $ Nval $ n * acc
    aggregate _ (Left err) = Left err
    aggreate notnum _ = Left $ (show notnum) ++ "is not a number"

{- todo: floats don't exist yet
biDivide :: [SchemeVal] -> Either String SchemeVal
biDivide ((Nval n):rest) = foldr aggregate 
                                    (Right $ Nval $ n) rest 
    where
    aggregate (Nval n) (Right (Nval acc)) = Right $ Nval $ n / acc
    aggregate _ (Left err) = Left err
    aggreate notnum _ = Left $ (show notnum) ++ "is not a number"
biDivide notnums = Left $ (show notnums) ++ "do not begin with a number"-}

evalFirst :: [SExpr] -> Either String (SchemeVal, [SExpr])
evalFirst [] = Left "tried to eval an empty list"
evalFirst (x:xs) = extract (eval x, xs) where
    extract (Left e, _) = Left e
    extract (Right a, b) = Right (a,b)

biIf :: [SExpr] -> Either String SchemeVal
biIf l@[x,y,z] = evalFirst l >>= listIf
    where listIf ((Boolean True),_)  = eval y
          listIf ((Boolean False),_) = eval z
          listIf (x,_) = Left $ (show x) ++ " is not a boolean"
biIf wronglen = Left $ (show wronglen) ++ " is the wrong length for an if statement"

biCons :: [SExpr] -> Either String SchemeVal
biCons [x, y] = liftM2 Pair (eval x) (eval y)
biCons nottwo = Left $ "cons was applied to the wrong number of arguments:" ++ (show nottwo)
biCar :: [SExpr] -> Either String SchemeVal
biCar [x] = eval x >>= unwrap where unwrap (Pair s1 _) = Right s1
                                    unwrap notpair = Left $ (show notpair) ++ " is not a pair"
biCar toomany = Left $ "car was applied to too many arguments" ++ (show toomany)
biCdr :: [SExpr] -> Either String SchemeVal
biCdr [x] = eval x >>= unwrap where unwrap (Pair _ s2) = Right s2
                                    unwrap notpair = Left $ (show notpair) ++ " is not a pair"
biCdr toomany = Left $ "cdr was applied to too many arguments" ++ (show toomany)
biAnd :: [SExpr] -> Either String SchemeVal
biAnd [] = Right $ Boolean True
biAnd [x] = eval x
biAnd (x:xs) = (eval x) >>= continue 
    where continue f@(Boolean False) = Right f
          continue truthy = biAnd xs
biOr :: [SExpr] -> Either String SchemeVal
biOr [] = Right $ Boolean False
biOr (x:xs) = (eval x) >>= continue 
    where continue f@(Boolean False) = biOr xs
          continue truthy = Right truthy

biQuote :: [SExpr] -> Either String SchemeVal
biQuote [] = Left "Quote called on too few arguments"
biQuote [A atom] = undefined
{-to write quote, it seems like we need some sort of intermediate 
representation, it's not a SchemeVal because it's not fully evaluated, but it's not an SExpr either . . . is it?
like scheme is weakly typed, the type of quote is, idk
-}
{- like is what's supposed to happen we parse the input into a series of nested lists and then we repeatedly call evaluate on all of those lists until we get something?
where `parseSExpr` makes the nested lists and then eval takes care of evaluating them?
-}
builtInsList :: [(Builtin, SchemeVal)]
builtInsList = [(Plus, Operator biPlus), 
                (Minus, Operator biMinus), 
                (Times, Operator biTimes),
                (If, Special biIf),
                (Cons, Special biCons),
                (Car, Special biCar),
                (Cdr, Special biCdr),
                (And, Special biAnd),
                (Or, Special biOr),
                (Quote, Special biQuote)]

{- eval needs to know the difference between a function, which
takes a list of evaluated arguments and evaluates them, and a 
"special form", which does something to the code-tree and then 
returns the rest of the code tree-}     
eval :: SExpr -> Either String SchemeVal
eval (A (N n)) = Right $ Nval n
eval (A (S str)) = Right $ Sval str
eval (A (BI b)) = wrap $ lookup b builtInsList where
    wrap Nothing = Left "builtin not found"
    wrap (Just func) = Right func
eval (A (BO bool)) = Right $ Boolean bool
eval (A (Null)) = Right EmptyList
eval (Comb list) =  (evalFirst list) >>= combEval
eval somethingelse = Left $ "got " ++ (show somethingelse)
--how to eval a macro goes here
combEval :: (SchemeVal, [SExpr]) -> Either String SchemeVal
combEval ((Operator func), rest) = (mapM eval rest) >>= func
combEval ((Macro m), rest) = (m rest) >>= (eval . Comb)
combEval ((Special s), rest) = s rest
combEval (x,rest) = Left $ "Tried to call " ++ (show x) ++ 
    " (not a function) on " ++ (show rest)

evaluateScheme :: String -> Either String SchemeVal
evaluateScheme = ((=<<) eval) . unpack . (runParser parseSExpr) 
    where unpack Nothing = Left "Parsing failed"
          unpack (Just (sexpr, "")) = Right sexpr
          unpack (Just (_, leftover)) = Left $ "parsed but had " ++ 
            leftover ++ "remaining at end"