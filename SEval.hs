

module SEval where

import Control.Monad
import Data.Semigroup
import Data.Bifunctor
import Data.Either.Combinators


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

--note that many of these built-ins do not support define inside 
--them, ie (if (define x #t) 5 6) returns 5 but does not define x

biIf :: (Context,[SExpr]) -> Either String (Context,SExpr)
biIf (c, [x,y,z]) = do  (cx, val) <- eval (c,x)
                        (case val of 
                            (BO True)  -> eval (cx,y)
                            (BO False) -> eval (cx,z)
                            x          -> Left $ (show x) 
                                        ++ " is not a boolean")
biIf (c, wronglen) = Left $ (show wronglen) ++ 
                        " is the wrong length for an if statement"

biCons :: (Context, [SExpr]) -> Either String (Context,SExpr)
biCons (c,[x, y]) = do  (cx,xval) <- eval (c,x) 
                        (cy,yval) <- eval (c,y)
                        return $ (cy, Comb [xval,yval])
biCons (c,nottwo) = Left $ 
            "cons was applied to the wrong number of arguments:"
            ++ (show nottwo)
            
biCar :: (Context,[SExpr]) -> Either String (Context,SExpr)
biCar (c, [x]) = do (cx,val) <- eval (c,x) 
                    case val of
                        (Comb (s1:_)) -> return (cx,s1)
                        notpair -> Left $ (show notpair) ++ 
                                    " is not a list"
biCar (c, toomany) = Left $ "car was applied to too many arguments" 
                        ++ (show toomany)
biCdr :: (Context,[SExpr]) -> Either String (Context,SExpr)
biCdr (c, [x]) = do (cx,val) <- eval (c,x)
                    case val of
                     (Comb (s1:s2)) -> Right $ (cx,Comb s2)
                     notpair -> Left $ (show notpair) 
                                ++ " is not a list"
biCdr (c, toomany) = Left $ "cdr was applied to too many arguments" 
                        ++ (show toomany)
biAnd :: (Context, [SExpr]) -> Either String (Context,SExpr)
biAnd (c, [])     = Right $ (c, BO True)
biAnd (c, [x])    = eval (c,x)
biAnd (c, (x:xs)) = do  (cx,val) <- eval (c,x)
                        case val of 
                            f@(BO False) -> Right (cx,f)
                            truthy       -> biAnd (cx,xs)
biOr :: (Context, [SExpr]) -> Either String (Context,SExpr)
biOr (c, []) = Right $ (c,BO False)
biOr (c, (x:xs)) = do  (cx,val) <- eval (c,x)
                       case val of
                        f@(BO False) -> biOr (cx, xs)
                        truthy       -> Right (cx,truthy)

biQuote :: (Context,[SExpr]) -> Either String (Context,SExpr)
biQuote (c,[]) = Left "Quote called on too few arguments"
biQuote (c, l) = Right $ (c, Comb l)

biLambda :: (Context,[SExpr]) -> Either String (Context,SExpr)
biLambda (c, [(Comb formals),body])
    | all isId formals = do formalIds <- mapM toIdent formals 
                            return $ (c, Closure c formalIds body)
    | otherwise        = Left $ show formals 
                        ++ "is not a valid list of formals"
    where isId (I _) = True
          isId _     = False
          toIdent (I ident) = Right ident
          toIdent notid     = Left $ (show notid) ++ 
                                "is not an identifier"
biLambda (_, []) = Left "lambda may not be called on nothing"
biLambda (_, [x]) = Left $ "lambda may not be called with formals " 
    ++ show x ++ " without a body"
biLambda (_, toomany) = Left $ "body of lambda:" ++ (show toomany) ++
                            "may only have one expression"
--because scheme is annoying, exps can be a list of expressions to
--evaluate one by one
biLet :: (Context, [SExpr]) -> Either String (Context,SExpr)
biLet (c, ((Comb defs):exps)) = do
    evDefs <- mapM extract defs
    innerc <- uncurry (bindArgs c) (unzip evDefs)
    (_, vals) <- evalSeq (c <> innerc,exps)
    return (c, last exps)
    where 
        extract (Comb [(I ident), expr ]) = Right (ident, expr)
        extract notdef = Left $ show notdef ++ 
                            "is not a definition"
biLet (_, [])  = Left "let may not be called on nothing" 
biLet (_, [x]) = Left $ "let called on too few arguments:" ++ show x
biLet (_, toomany) = Left $ "let called on too many arguments:" 
                            ++ show toomany
{- There are three contexts. 
The outer context is only used to evaluate the arguments.
The inner context is used to evaluate the body.
The formals extend the inner context. -}
--bindArgs takes the outer context, the formals to bind, and
--the list of arguments 
bindArgs :: Context -> Formals -> [SExpr] -> Either String Context
bindArgs c fs args | length fs == length args = 
    do eval'dArgs <- mapM (fmap snd . eval) (zip (repeat c) args)
       return $ C $ zip fs eval'dArgs
    | otherwise = Left $ "gave the wrong number of args " 
        ++ show args ++ " for the formals: " ++ show fs

biDefine :: (Context, [SExpr]) -> Either String (Context, SExpr)
biDefine (c@(C ctext), [I ident,y]) = case lookup ident ctext of
    Nothing  -> return $ (c <> C [(ident,y)], y)
    Just exp -> Left $ show ident ++ " has already been defined"
biDefine (_, exp) = Left $ "define " ++ show exp ++ 
                        " is not a valid define"
{-appendC :: Context -> Context -> Context
appendC (C c1) (C c2) = C $ c1 ++ c2-}

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
                ("lambda", Special biLambda),
                ("let", Special biLet),
                ("define", Special biDefine)]

{- eval needs to know the difference between a function, which
takes a list of evaluated arguments and evaluates them, and a 
"special form", which does something to the code-tree and then 
returns the rest of the code tree-}     
eval :: (Context,SExpr) -> Either String (Context,SExpr)
eval (c@(C context),(I token)) = case lookup token context of
    (Just func) -> eval (c, func)
    Nothing     -> Left $ (show token) ++ "not found"
eval (c,(Comb (x:xs))) = do (c2, evHead) <- eval (c,x)
                            combEval $ (c2, evHead:xs)
eval  (c, selfeval) = Right (c,selfeval)
{- I think all of this self-evaluates 
eval (c, val@(N n))     = Right (c, val)
eval (c, val@(S str))   = Right (c, val)
eval (c, val@(BO bool)) = Right (c, val)
eval (c, EmptyList)     = Right (c, EmptyList)
-}
{- sequentially evalutates the list of expressions, starting
in the given context and passing the context along as it goes -}
evalSeq :: (Context, [SExpr]) -> Either String (Context,[SExpr])
evalSeq (c,[x])  = do (newc, newx) <- eval (c,x) 
                      return (newc,[newx])
evalSeq (c,(x:xs)) = do (newc, xval) <- eval (c,x) 
                        (second $ (:) xval) <$> evalSeq (newc,xs)
                      
{-shareC :: (Context, [SExpr]) -> [(Context, SExpr)]
shareC (c, exps) = zip (repeat c) exps-}

combEval :: (Context,[SExpr]) -> Either String (Context,SExpr)
--(c,rest) :: (Context, [SExpr])
--eval  :: (Context, SExpr) -> Either String (Context, SExpr)
--func  :: ([SExpr]->Either String SExpr)
combEval (c,((F func):rest)) = do (newc, args) <- evalSeq (c,rest)
                                  exp <- func args
                                  return (newc, exp)
combEval (c, ((Macro m):rest)) = do newtree <- m rest
                                    eval (c,Comb newtree)
combEval (c, ((Special s):rest)) = s (c, rest)
combEval (c,((Closure innerc formals body):args)) =  
    do boundargs <- bindArgs c formals args
       eval ((boundargs <> innerc), body)
combEval (_, (x:rest)) = Left $ "Tried to call " ++ (show x) ++ 
    " (not a function) on " ++ (show rest)

evaluateScheme :: String -> Either String (Context, SExpr)
evaluateScheme s = case (runParser parseSExpr s) of
        Nothing -> Left "Parsing failed"
        (Just (sexpr, "")) -> eval (builtInsList, sexpr)
        (Just (_, leftover)) -> Left $ "parsed but had " ++ 
            leftover ++ "remaining at end"

parseProgram :: String -> Either String [SExpr]
parseProgram p = case mapM (runParser parseSExpr) $ lines p of 
    Nothing       -> Left "parsing failed"
    Just pairlist -> if all (null . snd) pairlist
        then Right $ fmap fst pairlist 
        else Left "parsing failed"
    

evalProgram :: String -> Either String [SExpr]
evalProgram p = parseProgram p >>= curry evalSeq builtInsList
            >>= return . snd