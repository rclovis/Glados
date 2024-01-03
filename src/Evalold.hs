{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Evalold
  ( emptyEnv,
    exec,
    prelude,
  )
where

import Astold (Ast (..))
import Control.Applicative
import qualified Data.Map as Map

-- | An environment is a mapping from a symbol
-- | with or without arguments to an S-Expression.
type Env = Map.Map String Ast

-- | An empty environment.
emptyEnv :: Env
emptyEnv = Map.empty

prelude :: Env
prelude =
  Map.fromList
    [ ("div", Lambda ["_a", "_b"] (Call (Op "/") [Var "_a", Var "_b"])),
      ("mod", Lambda ["_a", "_b"] (Call (Op "%") [Var "_a", Var "_b"])),
      ("eq?", Lambda ["_a", "_b"] (Call (Op "=") [Var "_a", Var "_b"]))
    ]

dispAtom :: Env -> Maybe Ast -> String
dispAtom _ Nothing = "Nothing"
dispAtom _ (Just (INum n)) = show n
dispAtom _ (Just (FNum n)) = show n
dispAtom _ (Just (Bool b)) = show b
dispAtom _ (Just (Str s)) = show s
dispAtom env (Just (Var s)) = case Map.lookup s env of
  Just expr -> dispAtom env (Just expr)
  Nothing -> error ("Could not find " <> s <> " in environment.")
dispAtom _ _ = error "Not an atom."

-- | Execute the program.
exec :: Env -> [Ast] -> IO ()
exec _ [] = putStrLn "Done."
exec env ((Define name body) : asts) = do
  let env' = Map.insert name body env
  exec env' asts
exec env [a] = putStrLn (dispAtom env $ eval env a)
exec env (ast : asts) = do
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
    <|> error ("Could not evaluate " <> show expr <> ".\nEnvironment: " <> show env <> ".")

evalNum :: Env -> Ast -> Maybe Ast
evalNum _ (INum n) = pure $ INum n
evalNum _ (FNum n) = pure $ FNum n
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
  expr <- Map.lookup s env
  eval env expr
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
  eval env (Call body args)
evalCall _ _ = Nothing

-- | Call an operator. May be logic or arithmetic.
callOp :: String -> [Ast] -> Maybe Ast
callOp "+" [INum a, INum b] = pure $ INum (a + b)
callOp "-" [INum a, INum b] = pure $ INum (a - b)
callOp "*" [INum a, INum b] = pure $ INum (a * b)
callOp "/" [INum a, INum b] = pure $ INum (a `div` b)
callOp "%" [INum a, INum b] = pure $ INum (a `mod` b)
callOp "<" [INum a, INum b] = pure $ Bool (a < b)
callOp ">" [INum a, INum b] = pure $ Bool (a > b)
callOp "<=" [INum a, INum b] = pure $ Bool (a <= b)
callOp ">=" [INum a, INum b] = pure $ Bool (a >= b)
callOp "=" [INum a, INum b] = pure $ Bool (a == b)
callOp "!=" [INum a, INum b] = pure $ Bool (a /= b)
callOp "&&" [Bool a, Bool b] = pure $ Bool (a && b)
callOp "||" [Bool a, Bool b] = pure $ Bool (a || b)
callOp "!" [Bool a] = pure $ Bool (not a)
callOp _ _ = Nothing
