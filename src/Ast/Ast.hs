{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ast.Ast
  ( Ast (..),
    genAst,
  )
where

import Ast.Expr (Expr (..))
import Ast.Op (Op (..))
import Ast.Types (Type (..))
import Ast.Utils (takeUntil)
import Control.Applicative
import Lexer (OperatorParsed (..), Token (..), TypeParsed (..))

type Arg = (String, Type)

data Ast
  = Seq [Ast]
  | Define String Type Ast
  | Assign String Ast
  | Lambda [Arg] Ast
  | Call String [Ast]
  | Return Ast
  | If Ast Ast Ast
  | While Ast Ast
  | Break
  | Continue
  | Id String
  | Value Ast
  | BinOp Op Ast Ast
  | UnOp Op Ast
  | Int Int
  | Float Float
  deriving (Eq, Ord, Show)

genAst :: [Expr] -> Maybe Ast
genAst xs = do
  (ast, []) <- getAst xs
  pure ast

getAst :: [Expr] -> Maybe (Ast, [Expr])
getAst [] = Nothing
getAst xs = do
  (ast, expr) <-
    getIf xs
      <|> getFunk xs
      <|> getCall xs
      <|> getIf xs
      <|> getDefine xs
      <|> getAssign xs
      <|> getReturn xs
      <|> getValue xs
      <|> getIdentifier xs
      <|> error ("Not implemented: " ++ show xs)
  case getAst expr of
    Nothing -> pure (ast, expr)
    Just (ys, zs) -> pure (Seq (ast : [ys]), zs)

getValue :: [Expr] -> Maybe (Ast, [Expr])
getValue xs = do
  (ast, expr) <-
    getCall xs
      <|> getPriority xs
      <|> getCall xs
      <|> getBinOp xs
      <|> getUnOp xs
      <|> getIdentifier xs
      <|> getNumber xs
      <|> error ("Invalid value: " ++ show xs)
  case getAst expr of
    Nothing -> pure (ast, expr)
    Just (ys, zs) -> pure (Seq (ast : [ys]), zs)

getFunk :: [Expr] -> Maybe (Ast, [Expr])
getFunk (A Funk : A (Identifier name) : Parenthesis args : A (Symbol ":") : A (Lexer.Type t) : Braces body : xs) = do
  (args', []) <- getArgs args
  (body', []) <- getAst body
  type' <- getType t
  pure (Define name type' (Lambda args' body'), xs)
  where
    getArgs :: [Expr] -> Maybe ([Arg], [Expr])
    getArgs [] = pure ([], [])
    getArgs (A (Identifier name') : A (Symbol ":") : A (Lexer.Type t') : zs) = do
      t'' <- getType t'
      (args', []) <- getArgs zs
      pure ((name', t'') : args', [])
    getArgs _ = Nothing
getFunk _ = Nothing

getIf :: [Expr] -> Maybe (Ast, [Expr])
getIf (A Lexer.If : Parenthesis cond : Braces thenBody : A Lexer.Else : Braces elseBody : xs) = do
  (cond', []) <- getAst cond
  (thenBody', []) <- getAst thenBody
  (elseBody', []) <- getAst elseBody
  pure (Ast.Ast.If cond' thenBody' elseBody', xs)
getIf (A Lexer.If : Parenthesis cond : Braces body : xs) = do
  (cond', []) <- getAst cond
  (body', []) <- getAst body
  pure (Ast.Ast.If cond' body' (Seq []), xs)
getIf _ = Nothing

getCall :: [Expr] -> Maybe (Ast, [Expr])
getCall (FuncCall name (Parenthesis args) : A End : xs) = do
  (args', []) <- getValue args
  pure (Call name [args'], xs)
getCall (FuncCall name (Parenthesis args) : xs) = do
  (args', []) <- getValue args
  pure (Call name [args'], xs)
getCall _ = Nothing

getPriority :: [Expr] -> Maybe (Ast, [Expr])
getPriority (Parenthesis expr : xs) = do
  (ast, []) <- getAst expr
  pure (ast, xs)
getPriority _ = Nothing

getDefine :: [Expr] -> Maybe (Ast, [Expr])
getDefine (A Lexer.Var : A (Identifier name) : A (Symbol ":") : A (Lexer.Type t) : A (Symbol "=") : xs) = do
  let (value, xs') = takeUntil (== A End) xs
  (expr, _) <- getAst value
  t' <- getType t
  pure (Define name t' expr, xs')
getDefine _ = Nothing

getAssign :: [Expr] -> Maybe (Ast, [Expr])
getAssign (A (Identifier name) : A (Symbol "=") : xs) = do
  let (value, xs') = takeUntil (== A End) xs
  (expr, _) <- getAst value
  pure (Assign name expr, xs')
getAssign _ = Nothing

getReturn :: [Expr] -> Maybe (Ast, [Expr])
getReturn (A (Identifier "return") : xs) = do
  let (value, xs') = takeUntil (== A End) xs
  (expr, _) <- getAst value
  pure (Return expr, xs')
getReturn _ = Nothing

getBinOp :: [Expr] -> Maybe (Ast, [Expr])
getBinOp (a : A (Operator op) : b : xs) = do
  (a', []) <- getAst [a]
  (b', []) <- getAst [b]
  op' <- Ast.Ast.getOp op
  pure (BinOp op' a' b', xs)
getBinOp _ = Nothing

getUnOp :: [Expr] -> Maybe (Ast, [Expr])
getUnOp (A (Operator op) : a : xs) = do
  (a', []) <- getAst [a]
  op' <- Ast.Ast.getOp op
  pure (UnOp op' a', xs)
getUnOp _ = Nothing

getNumber :: [Expr] -> Maybe (Ast, [Expr])
getNumber (A (INumber n) : xs) = pure (Int n, xs)
getNumber (A (FNumber n) : xs) = pure (Float n, xs)
getNumber _ = Nothing

getIdentifier :: [Expr] -> Maybe (Ast, [Expr])
getIdentifier (A (Identifier name) : xs) = pure (Id name, xs)
getIdentifier _ = Nothing

getOp :: OperatorParsed -> Maybe Ast.Op.Op
getOp Lexer.Add = pure Ast.Op.Add
getOp Lexer.Sub = pure Ast.Op.Sub
getOp Lexer.Mul = pure Ast.Op.Mul
getOp Lexer.Div = pure Ast.Op.Div
getOp Lexer.Mod = pure Ast.Op.Mod
getOp Lexer.Equal = pure Ast.Op.Eq
getOp Lexer.NotEqual = pure Ast.Op.Neq
getOp Lexer.Less = pure Ast.Op.Lt
getOp Lexer.Greater = pure Ast.Op.Gt
getOp Lexer.LessEqual = pure Ast.Op.Le
getOp Lexer.GreaterEqual = pure Ast.Op.Ge
getOp Lexer.And = pure Ast.Op.And
getOp Lexer.Or = pure Ast.Op.Or
getOp Lexer.Not = pure Ast.Op.Not

getType :: TypeParsed -> Maybe Type
getType I8 = pure Ti8
getType I16 = pure Ti16
getType I32 = pure Ti32
getType I64 = pure Ti64
getType U8 = pure Tu8
getType U16 = pure Tu16
getType U32 = pure Tu32
getType U64 = pure Tu64
getType F32 = pure Tf32
getType F64 = pure Tf64
getType _ = Nothing
