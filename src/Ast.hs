{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ast () where

import Control.Applicative ((<|>))
import Sexpr (Sexpr (..))

data Ast
  = Define String Ast
  | If Ast Ast Ast
  | Lambda [String] Ast
  | Call Ast [Ast]
  | Var String
  | Num Integer
  | Str String
  | Bool Bool
  | Sym String
  deriving (Eq, Ord, Show)

-- | Factorial function AST.
runFactorial :: [Ast]
runFactorial =
  [ Define
      "factorial"
      ( Lambda
          ["n"]
          ( If
              (Call (Var "<") [Var "n", Ast.Num 2])
              (Ast.Num 1)
              (Call (Var "*") [Var "n", Call (Var "factorial") [Call (Var "-") [Var "n", Ast.Num 1]]])
          )
      ),
    Call (Var "factorial") [Ast.Num 5]
  ]

-- | Fibonacci function AST.
runFibonacci :: [Ast]
runFibonacci =
  [ Define
      "fibonacci"
      ( Lambda
          ["n"]
          ( If
              (Call (Var "<") [Var "n", Ast.Num 2])
              (Ast.Num 1)
              (Call (Var "+") [Call (Var "fibonacci") [Call (Var "-") [Var "n", Ast.Num 1]], Call (Var "fibonacci") [Call (Var "-") [Var "n", Ast.Num 2]]])
          )
      ),
    Call (Var "fibonacci") [Ast.Num 10]
  ]

getAst :: Sexpr -> Maybe Ast
getAst expr =
  getNum expr
    <|> getSym expr
    <|> getVar expr
    <|> getBool expr
    <|> getStr expr
    <|> getDefine expr
    <|> getIf expr
    <|> getCall expr

-- | Gen program
genAst :: [Sexpr] -> Maybe [Ast]
genAst = traverse getAst

-- | Get a number from an SExpr.
getNum :: Sexpr -> Maybe Ast
getNum (Sexpr.Num n) = pure (Ast.Num n)
getNum _ = Nothing

-- | Get a string from an SExpr.
getSym :: Sexpr -> Maybe Ast
getSym (Sexpr.Sym "") = Nothing
getSym (Sexpr.Sym s@"define") = pure (Ast.Sym s)
getSym s = getVar s

getVar :: Sexpr -> Maybe Ast
getVar (Sexpr.Sym "") = Nothing
getVar (Sexpr.Sym s) = pure (Ast.Var s)
getVar _ = Nothing

-- | Get a boolean from an SExpr.
getBool :: Sexpr -> Maybe Ast
getBool (Sexpr.Bool b) = pure (Ast.Bool b)
getBool _ = Nothing

-- | Get a string from an SExpr.
getStr :: Sexpr -> Maybe Ast
getStr (Sexpr.Str s) = pure (Ast.Str s)
getStr _ = Nothing

-- | Get a list of arguments from an SExpr.
getArgs :: [Sexpr] -> Maybe [String]
getArgs = traverse getArg
  where
    getArg (Sexpr.Sym "") = Nothing
    getArg (Sexpr.Sym s) = pure s
    getArg _ = Nothing

getDefine :: Sexpr -> Maybe Ast
getDefine (Sexpr.List [Sexpr.Sym "define", Sexpr.List (Sexpr.Sym name : args), expr]) =
  Ast.Define name <$> (Ast.Lambda <$> getArgs args <*> getAst expr)
getDefine (Sexpr.List [Sexpr.Sym "define", Sexpr.Sym name, expr]) = Ast.Define name <$> getAst expr
getDefine _ = Nothing

getIf :: Sexpr -> Maybe Ast
getIf (Sexpr.List [Sexpr.Sym "if", cond, thenExpr, elseExpr]) =
  Ast.If <$> getAst cond <*> getAst thenExpr <*> getAst elseExpr
getIf _ = Nothing

getCall :: Sexpr -> Maybe Ast
getCall (Sexpr.List (name : args)) = Ast.Call <$> getSym name <*> traverse getAst args
getCall _ = Nothing

factorial :: [Sexpr]
factorial =
  [ List
      [ Sexpr.Sym "define",
        List [Sexpr.Sym "factorial", Sexpr.Sym "n"],
        List
          [ Sexpr.Sym "if",
            List
              [ Sexpr.Sym "=",
                Sexpr.Sym "n",
                Sexpr.Num 0
              ],
            Sexpr.Num 1,
            List
              [ Sexpr.Sym "*",
                Sexpr.Sym "n",
                List
                  [ Sexpr.Sym "factorial",
                    List [Sexpr.Sym "-", Sexpr.Sym "n", Sexpr.Num 1]
                  ]
              ]
          ]
      ],
    List
      [Sexpr.Sym "factorial", Sexpr.Num 10]
  ]
