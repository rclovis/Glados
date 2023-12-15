{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Eval () where

import Ast (Ast (..))
import Control.Applicative
import qualified Data.Map as Map

-- | An environment is a mapping from a symbol
-- | with or without arguments to an S-Expression.
type Env = Map.Map String Ast

-- | An empty environment.
emptyEnv :: Env
emptyEnv = Map.empty

-- | Execute the program.
exec :: Env -> [Ast] -> IO ()
exec _ [] = putStrLn "Done."
exec env ((Define name body) : asts) = do
  let env' = Map.insert name body env
  _ <- putStrLn $ "Defined " <> name
  exec env' asts
exec env (ast : asts) = do
  print (eval env ast)
  exec env asts

-- | Evaluate an Ast.
eval :: Env -> Ast -> Maybe Ast
eval env expr =
  evalCall env expr
    <|> evalIf env expr
    <|> evalNum env expr
    <|> evalBool env expr
    <|> evalStr env expr
    <|> evalVar env expr

evalNum :: Env -> Ast -> Maybe Ast
evalNum _ (Num n) = pure $ Num n
evalNum _ _ = Nothing

evalBool :: Env -> Ast -> Maybe Ast
evalBool _ (Bool b) = pure $ Bool b
evalBool _ _ = Nothing

evalStr :: Env -> Ast -> Maybe Ast
evalStr _ (Str s) = pure $ Str s
evalStr _ _ = Nothing

-- | Evaluate a variable in the environment.
evalVar :: Env -> Ast -> Maybe Ast
evalVar env (Var s) = do
  Map.lookup s env
evalVar _ _ = Nothing

-- | Evaluate an if expression.
evalIf :: Env -> Ast -> Maybe Ast
evalIf env (If condExpr thenExpr elseExpr) = do
  Bool b <- eval env condExpr
  if b then eval env thenExpr else eval env elseExpr
evalIf _ _ = Nothing

-- | Evaluate a call. May be a call to a function, an operator or a lambda.
evalCall :: Env -> Ast -> Maybe Ast
evalCall env (Call (Op op) args) = do
  args' <- traverse (eval env) args
  callOp op args'
evalCall env (Call (Lambda params body) args) = do
  args' <- traverse (eval env) args
  let env' = Map.fromList (zip params args') <> env
  eval env' body
evalCall env (Call (Var s) args) = do
  body <- Map.lookup s env
  args' <- traverse (eval env) args
  eval env (Call body args')
evalCall _ _ = Nothing

-- | Call an operator. May be logic or arithmetic.
callOp :: String -> [Ast] -> Maybe Ast
callOp "+" [Num a, Num b] = pure $ Num (a + b)
callOp "-" [Num a, Num b] = pure $ Num (a - b)
callOp "*" [Num a, Num b] = pure $ Num (a * b)
callOp "/" [Num a, Num b] = pure $ Num (a `div` b)
callOp "<" [Num a, Num b] = pure $ Bool (a < b)
callOp ">" [Num a, Num b] = pure $ Bool (a > b)
callOp "<=" [Num a, Num b] = pure $ Bool (a <= b)
callOp ">=" [Num a, Num b] = pure $ Bool (a >= b)
callOp "==" [Num a, Num b] = pure $ Bool (a == b)
callOp "!=" [Num a, Num b] = pure $ Bool (a /= b)
callOp "&&" [Bool a, Bool b] = pure $ Bool (a && b)
callOp "||" [Bool a, Bool b] = pure $ Bool (a || b)
callOp "!" [Bool a] = pure $ Bool (not a)
callOp _ _ = Nothing
