

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

biIf :: Context->[SExpr] -> Either String SExpr
biIf c [x,y,z] = eval c x >>= listIf
    where listIf (BO True)  = eval c y
          listIf (BO False) = eval c z
          listIf x = Left $ (show x) ++ " is not a boolean"
biIf c wronglen = Left $ (show wronglen) ++ " is the wrong length for an if statement"

biCons :: Context -> [SExpr] -> Either String SExpr
biCons c [x, y] = do xval <- eval c x 
                     yval <- eval c y
                     return $ Comb [xval,yval]
biCons c nottwo = Left $ "cons was applied to the wrong number of arguments:" ++ (show nottwo)
biCar :: Context -> [SExpr] -> Either String SExpr
biCar c [x] = eval c x >>= unwrap where 
    unwrap (Comb (s1:_)) = Right s1
    unwrap notpair = Left $ (show notpair) ++ " is not a list"
biCar c toomany = Left $ "car was applied to too many arguments" ++ (show toomany)
biCdr :: Context -> [SExpr] -> Either String SExpr
biCdr c [x] = eval c x >>= unwrap where 
    unwrap (Comb (s1:s2)) = Right $ Comb s2
    unwrap notpair = Left $ (show notpair) ++ " is not a list"
biCdr c toomany = Left $ "cdr was applied to too many arguments" ++ (show toomany)
biAnd :: Context -> [SExpr] -> Either String SExpr
biAnd c [] = Right $ BO True
biAnd c [x] = eval c x
biAnd c (x:xs) = (eval c x) >>= continue 
    where continue f@(BO False) = Right f
          continue truthy = biAnd c xs
biOr :: Context -> [SExpr] -> Either String SExpr
biOr c [] = Right $ BO False
biOr c (x:xs) = (eval c x) >>= continue 
    where continue f@(BO False) = biOr c xs
          continue truthy = Right truthy

biQuote :: Context -> [SExpr] -> Either String SExpr
biQuote _ [] = Left "Quote called on too few arguments"
biQuote _ l = Right $ Comb l

biLambda :: Context -> [SExpr] -> Either String SExpr
biLambda _ [] = Left "lambda may not be called on nothing"
biLambda _ [x] = Left $ "lambda may not be called with formals " 
    ++ show x ++ " without a body"
biLambda c [(Comb formals),body]
    | all isId formals = do formalIds <- mapM toIdent formals 
                            return $ Closure c formalIds body
    | otherwise     = Left $ show formals 
                        ++ "is not a valid list of formals"
    where isId (I _) = True
          isId _     = False
          toIdent (I ident) = Right ident
          toIdent notid     = Left $ (show notid) ++ 
                                "is not an identifier"
biLambda _ toomany = Left $ "body of lambda:" ++ (show toomany) ++
                            "may only have one expression"
{- There are three contexts. 
The outer context is only used to evaluate the arguments.
The inner context is used to evaluate the body.
The formals extend the inner context. -}
--bindArgs takes the outer context, the formals to bind, and
--the list of arguments 
bindArgs :: Context -> Formals -> [SExpr] -> Either String Context
bindArgs c fs args | length fs == length args = 
    do eval'dArgs <- mapM (eval c) args
       return $ C $ zip fs eval'dArgs
    | otherwise = Left $ "gave the wrong number of args " 
        ++ show args ++ " for the formals: " ++ show fs

appendC :: Context -> Context -> Context
appendC (C c1) (C c2) = C $ c1 ++ c2

{-to write quote, it seems like we need some sort of intermediate 
representation, it's not a SchemeVal because it's not fully evaluated, but it's not an SExpr either . . . is it?
like scheme is weakly typed, the type of quote is, idk
-}
{- like is what's supposed to happen we parse the input into a series of nested lists and then we repeatedly call evaluate on all of those lists until we get something?
where `parseSExpr` makes the nested lists and then eval takes care of evaluating them?
-}


builtInsList :: Context
builtInsList = C [("+", F biPlus), 
                ("-", F biMinus), 
                ("*", F biTimes),
                ("if", Special biIf),
                ("cons", Special biCons),
                ("car", Special biCar),
                ("cdr", Special biCdr),
                ("and", Special biAnd),
                ("or", Special biOr),
                ("quote", Special biQuote),
                ("lambda", Special biLambda)]

{- eval needs to know the difference between a function, which
takes a list of evaluated arguments and evaluates them, and a 
"special form", which does something to the code-tree and then 
returns the rest of the code tree-}     
eval :: Context -> SExpr -> Either String SExpr
eval _ val@(N n) = Right val
eval _ val@(S str) = Right val
eval _ val@(BO bool) = Right val
eval _ EmptyList = Right EmptyList
eval (C context) (I token) = wrap $ lookup token context where
    wrap Nothing = Left $ (show token) ++ "not found"
    wrap (Just func) = Right func
eval c (Comb (x:xs)) =  ((:) <$> eval c x <*> pure xs) 
                                   >>= (combEval c)
eval  _ somethingelse = Left $ "got " ++ (show somethingelse)

combEval :: Context -> [SExpr] -> Either String SExpr
combEval c ((F func):rest) = (mapM (eval c) rest) >>= func
combEval c ((Macro m):rest)
        = (m rest) >>= (eval c . Comb)
combEval ctext ((Special s):rest) = s ctext rest
combEval c ((Closure innerc formals body):args) =  
    do boundargs <- bindArgs c formals args
       eval (appendC boundargs innerc) body
combEval _ (x:rest) = Left $ "Tried to call " ++ (show x) ++ 
    " (not a function) on " ++ (show rest)

evaluateScheme :: String -> Either String SExpr
evaluateScheme = ((=<<) (eval builtInsList)) . unpack . (runParser parseSExpr) 
    where unpack Nothing = Left "Parsing failed"
          unpack (Just (sexpr, "")) = Right sexpr
          unpack (Just (_, leftover)) = Left $ "parsed but had " ++ 
            leftover ++ "remaining at end"