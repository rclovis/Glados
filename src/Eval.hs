{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Eval () where

import qualified Data.Map as Map

-- | An environment is a mapping from a symbol
-- | with or without arguments to an S-Expression.
type Env = Map.Map String ([String], SExpr)

-- | An S-Expression is either a number, a symbol, or a list of S-Expressions.
data SExpr
  = Num Integer
  | Str String
  | Bool Bool
  | Sym String
  | List [SExpr]
  deriving (Eq, Ord, Show)

-- | A sample S-Expression printing the number 42.
runPrintFoo :: [SExpr]
runPrintFoo =
  [ List [Sym "define", Sym "foo", Num 42],
    List [Sym "print", Sym "foo"]
  ]

-- | A sample S-Expression printing with a conditional.
runPrintFooCond :: [SExpr]
runPrintFooCond =
  [ List [Sym "define", Sym "foo", Num 42],
    List
      [ Sym "if",
        List [Sym "<", Sym "foo", Num 10],
        List [Sym "print", Str "foo is less than 10"],
        List [Sym "print", Str "foo is greater than 10"]
      ]
  ]

-- | A sample S-Expression representing the factorial function.
runFactorial :: [SExpr]
runFactorial =
  [ List
      [ Sym "define",
        List [Sym "factorial", Sym "n"],
        List
          [ Sym "if",
            List
              [ Sym "=",
                Sym "n",
                Num 0
              ],
            Num 1,
            List
              [ Sym "*",
                Sym "n",
                List
                  [ Sym "factorial",
                    List [Sym "-", Sym "n", Num 1]
                  ]
              ]
          ]
      ],
    List
      [Sym "factorial", Num 10]
  ]

-- | Run a list of instructions representing the program.
run :: Env -> [SExpr] -> IO ()
run _ [] = putStrLn "Execution finished!"
run env (x : xs) = do
  env' <- exec env x
  run env' xs

-- | Execute a single instruction.
exec :: Env -> SExpr -> IO Env
exec env (List [Sym "define", Sym name, expr]) = do
  return $ Map.insert name ([], expr) env
exec env (List [Sym "define", List (Sym name : args), expr]) = do
  return $ Map.insert name (map symToStr args, expr) env
exec env (List [Sym "print", expr]) = do
  result <- eval env expr
  putStrLn $ sexprToStr result
  return env
exec env x = do
  result <- eval env x
  putStrLn $ "Result: " ++ show result
  return env

getNumValue :: SExpr -> Integer
getNumValue (Num n) = n
getNumValue x = error $ "Invalid number: " ++ show x

-- | Get the S-Expression value of a defined symbol in the environment.
eval :: Env -> SExpr -> IO SExpr
eval env (Sym name) = do
  case Map.lookup name env of
    Just (_, expr) -> return expr
    Nothing -> error $ "Undefined symbol: " ++ name
eval env (List [Sym "if", cond, thenExpr, elseExpr]) = do
  cond' <- evalCond env cond
  case cond' of
    Bool True -> eval env thenExpr
    Bool False -> eval env elseExpr
    _ -> error "Condition must be a boolean"
eval env (List [Sym "print", expr]) = do
  result <- eval env expr
  putStrLn $ sexprToStr result
  return result
eval env (List [Sym "+", a, b]) = do
  a' <- eval env a
  b' <- eval env b
  return $ Num $ getNumValue a' + getNumValue b'
eval env (List [Sym "-", a, b]) = do
  a' <- eval env a
  b' <- eval env b
  return $ Num $ getNumValue a' - getNumValue b'
eval env (List [Sym "*", a, b]) = do
  a' <- eval env a
  b' <- eval env b
  return $ Num $ getNumValue a' * getNumValue b'
eval env (List [Sym "/", a, b]) = do
  a' <- eval env a
  b' <- eval env b
  return $ Num $ getNumValue a' `div` getNumValue b'
eval env (List (Sym f : args)) = do
  case Map.lookup f env of
    Just (params, expr) -> do
      args' <- mapM (eval env) args
      let env' = Map.fromList $ zip params $ map ([],) args'
      let env'' = Map.union env' env
      eval env'' expr
    Nothing -> error $ "Undefined function: " ++ f
eval _ x = return x

-- | Evaluate a conditional expression.
evalCond :: Env -> SExpr -> IO SExpr
evalCond env (List [Sym op, a, b]) = do
  a' <- eval env a
  b' <- eval env b
  return $ Bool $ opToFunc op a' b'
evalCond _ x = error $ "Invalid conditional: " ++ show x

-- | Map operator symbols to corresponding comparison functions.
opToFunc :: String -> (SExpr -> SExpr -> Bool)
opToFunc "<" = (<)
opToFunc "=" = (==)
opToFunc ">" = (>)
opToFunc "<=" = (<=)
opToFunc ">=" = (>=)
opToFunc "!=" = (/=)
opToFunc op = error $ "Invalid operator: " ++ op

-- | Map operator symbols to corresponding arithmetic functions.
opToFuncArith :: String -> (Integer -> Integer -> Integer)
opToFuncArith "+" = (+)
opToFuncArith "-" = (-)
opToFuncArith "*" = (*)
opToFuncArith "/" = div
opToFuncArith op = error $ "Invalid operator: " ++ op

-- | Get String from Symbol.
symToStr :: SExpr -> String
symToStr (Sym s) = s
symToStr x = error $ "Invalid symbol: " ++ show x

-- | Get string from S-Expression.
sexprToStr :: SExpr -> String
sexprToStr (Num n) = show n
sexprToStr (Str s) = s
sexprToStr (Bool b) = show b
sexprToStr (Sym s) = s
sexprToStr (List xs) = "(" ++ unwords (map sexprToStr xs) ++ ")"
