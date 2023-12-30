{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ast
  ( Ast (..),
    genAst,
    factorial,
  )
where

import Control.Applicative ((<|>))
import Sexpr (Sexpr (..))

data Ast
  = Define String Ast
  | If Ast Ast Ast
  | Lambda [String] Ast
  | Call Ast [Ast]
  | Sym String
  | Var String
  | Str String
  | Op String
  | INum Int
  | FNum Float
  | Bool Bool
  deriving (Eq, Ord, Show)

getAst :: Sexpr -> Maybe Ast
getAst expr =
  getNum expr
    <|> getOp expr
    <|> getSym expr
    <|> getVar expr
    <|> getBool expr
    <|> getStr expr
    <|> getDefine expr
    <|> getIf expr
    <|> getLamdba expr
    <|> getCall expr

-- | Gen program
genAst :: [Sexpr] -> Maybe [Ast]
genAst = traverse getAst

-- | Get a number from an SExpr.
getNum :: Sexpr -> Maybe Ast
getNum (Sexpr.INum n) = pure (Ast.INum n)
getNum (Sexpr.FNum n) = pure (Ast.FNum n)
getNum _ = Nothing

getSym :: Sexpr -> Maybe Ast
getSym (Sexpr.Sym "") = Nothing
getSym (Sexpr.Sym s@"define") = pure (Ast.Sym s)
getSym _ = Nothing

getVar :: Sexpr -> Maybe Ast
getVar (Sexpr.Sym "") = Nothing
getVar (Sexpr.Sym s) = pure (Ast.Var s)
getVar _ = Nothing

getOp :: Sexpr -> Maybe Ast
getOp (Sexpr.Sym "") = Nothing
getOp (Sexpr.Sym s@"+") = pure (Ast.Op s)
getOp (Sexpr.Sym s@"-") = pure (Ast.Op s)
getOp (Sexpr.Sym s@"*") = pure (Ast.Op s)
getOp (Sexpr.Sym s@"/") = pure (Ast.Op s)
getOp (Sexpr.Sym s@"=") = pure (Ast.Op s)
getOp (Sexpr.Sym s@"!=") = pure (Ast.Op s)
getOp (Sexpr.Sym s@"<") = pure (Ast.Op s)
getOp (Sexpr.Sym s@">") = pure (Ast.Op s)
getOp (Sexpr.Sym s@"<=") = pure (Ast.Op s)
getOp (Sexpr.Sym s@">=") = pure (Ast.Op s)
getOp (Sexpr.Sym s@"&&") = pure (Ast.Op s)
getOp (Sexpr.Sym s@"||") = pure (Ast.Op s)
getOp (Sexpr.Sym s@"!") = pure (Ast.Op s)
getOp _ = Nothing

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
getCall (Sexpr.List (name : args)) = Ast.Call <$> getAst name <*> traverse getAst args
getCall _ = Nothing

getLamdba :: Sexpr -> Maybe Ast
getLamdba (Sexpr.List [Sexpr.Sym "lambda", Sexpr.List args, expr]) =
  Ast.Lambda <$> getArgs args <*> getAst expr
getLamdba _ = Nothing

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
                Sexpr.INum 0
              ],
            Sexpr.INum 1,
            List
              [ Sexpr.Sym "*",
                Sexpr.Sym "n",
                List
                  [ Sexpr.Sym "factorial",
                    List [Sexpr.Sym "-", Sexpr.Sym "n", Sexpr.INum 1]
                  ]
              ]
          ]
      ],
    List
      [Sexpr.Sym "factorial", Sexpr.INum 10]
  ]
