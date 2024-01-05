{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ast
  ( Ast (..),
    Ast.Op (..),
    Type (..),
    genAst,
  )
where

import Control.Applicative
import Expr (Expr (..))
import Lexer (OperatorParsed (..), Token (..), TypeParsed (..))

data Op = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Le | Ge | And | Or | Not
  deriving (Eq, Ord, Show)

data Type = Ti8 | Ti16 | Ti32 | Ti64 | Tu8 | Tu16 | Tu32 | Tu64 | Tf32 | Tf64
  deriving (Eq, Ord, Show)

type Arg = (String, Type)

data Ast
  = Seq [Ast]
  | Print Ast
  | Define String Type Ast
  | Assign String Ast
  | Lambda [Arg] Ast
  | Call String [Ast]
  | If Ast Ast Ast
  | While Ast Ast
  | Break
  | Id String
  | BinOp Ast.Op Ast Ast
  | UnOp Ast.Op Ast
  | Float Float
  | Int Int
  | Null
  deriving (Eq, Ord, Show)

genAst :: Expr -> Maybe Ast
genAst (Start xs) = do
  (ast, []) <- getAst xs
  pure ast
genAst _ = Nothing

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
      <|> getBinOp xs
      <|> getUnOp xs
      <|> getValue xs
      <|> getIdentifier xs
      <|> getNumber xs
      <|> error ("Not implemented: " ++ show xs)
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
  pure (Ast.If cond' thenBody' elseBody', xs)
getIf (A Lexer.If : Parenthesis cond : Braces body : xs) = do
  (cond', []) <- getAst cond
  (body', []) <- getAst body
  pure (Ast.If cond' body' (Seq []), xs)
getIf _ = Nothing

getCall :: [Expr] -> Maybe (Ast, [Expr])
getCall (A (Identifier name) : Parenthesis args : xs) = do
  (args', []) <- getValue args
  pure (Call name [args'], xs)
getCall _ = Nothing

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
      <|> getBool xs
      <|> getString xs
      <|> error ("Invalide value: " ++ show xs)
  case getAst expr of
    Nothing -> pure (ast, expr)
    Just (ys, zs) -> pure (Seq (ast : [ys]), zs)

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

getBinOp :: [Expr] -> Maybe (Ast, [Expr])
getBinOp (a : A (Operator op) : b : xs) = do
  (a', []) <- getAst [a]
  (b', []) <- getAst [b]
  op' <- Ast.getOp op
  pure (BinOp op' a' b', xs)
getBinOp _ = Nothing

getUnOp :: [Expr] -> Maybe (Ast, [Expr])
getUnOp (A (Operator op) : a : xs) = do
  (a', []) <- getAst [a]
  op' <- Ast.getOp op
  pure (UnOp op' a', xs)
getUnOp _ = Nothing

getNumber :: [Expr] -> Maybe (Ast, [Expr])
getNumber (A (INumber n) : xs) = pure (Int n, xs)
getNumber (A (FNumber n) : xs) = pure (Float n, xs)
getNumber _ = Nothing

getString :: [Expr] -> Maybe (Ast, [Expr])
getString (A (Lexer.String s) : xs) = pure (Str s, xs)
getString _ = Nothing

getIdentifier :: [Expr] -> Maybe (Ast, [Expr])
getIdentifier (A (Identifier name) : xs) = pure (Id name, xs)
getIdentifier _ = Nothing

getBool :: [Expr] -> Maybe (Ast, [Expr])
getBool (A (Lexer.Boolean b) : xs) = pure (Ast.Bool b, xs)
getBool _ = Nothing

takeUntil :: (a -> Bool) -> [a] -> ([a], [a])
takeUntil _ [] = ([], [])
takeUntil f (x : xs)
  | f x = ([], xs)
  | otherwise = (x : ys, zs)
  where
    (ys, zs) = takeUntil f xs

getOp :: OperatorParsed -> Maybe Ast.Op
getOp Lexer.Add = pure Ast.Add
getOp Lexer.Sub = pure Ast.Sub
getOp Lexer.Mul = pure Ast.Mul
getOp Lexer.Div = pure Ast.Div
getOp Lexer.Mod = pure Ast.Mod
getOp Lexer.Equal = pure Ast.Eq
getOp Lexer.NotEqual = pure Ast.Neq
getOp Lexer.Less = pure Ast.Lt
getOp Lexer.Greater = pure Ast.Gt
getOp Lexer.LessEqual = pure Ast.Le
getOp Lexer.GreaterEqual = pure Ast.Ge
getOp Lexer.And = pure Ast.And
getOp Lexer.Or = pure Ast.Or
getOp Lexer.Not = pure Ast.Not

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
getType Lexer.Bool = pure Tbool
getType _ = Nothing
