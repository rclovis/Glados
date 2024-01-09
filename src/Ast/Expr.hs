{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ast.Expr
  ( Expr (..),
    genExpr,
  )
where

import Lexer (Token (..))

data Expr
  = Parenthesis [Expr]
  | Braces [Expr]
  | Brackets [Expr]
  | FuncCall String Expr
  | A Token
  deriving (Eq, Show)

genExpr :: [Token] -> Maybe [Expr]
genExpr [] = Nothing
genExpr tokens = do
  (expr, []) <- safeGenGroup tokens
  let (expr', _) = getFuncCall expr
  pure expr'

getFuncCall :: [Expr] -> ([Expr], [Expr])
getFuncCall [] = ([], [])
getFuncCall (a@(A Funk) : b@(A (Identifier _)) : xs) = do
  let (expr, ys) = getFuncCall xs
  (a : b : expr, ys)
getFuncCall (A (Identifier name) : (Parenthesis args) : xs) = do
  let (expr, ys) = getFuncCall xs
  let (expr', _) = getFuncCall args
  (FuncCall name (Parenthesis expr') : expr, ys)
getFuncCall (Parenthesis args : xs) = do
  let (expr, ys) = getFuncCall xs
  let (expr', _) = getFuncCall args
  (Parenthesis expr' : expr, ys)
getFuncCall (Braces args : xs) = do
  let (expr, ys) = getFuncCall xs
  let (expr', _) = getFuncCall args
  (Braces expr' : expr, ys)
getFuncCall (Brackets args : xs) = do
  let (expr, ys) = getFuncCall xs
  let (expr', _) = getFuncCall args
  (Brackets expr' : expr, ys)
getFuncCall (x : xs) = do
  let (expr, ys) = getFuncCall xs
  (x : expr, ys)

genGroup :: [Token] -> ([Expr], [Token])
genGroup [] = ([], [])
genGroup (CloseBracket : xs) = ([], xs)
genGroup (OpenBracket : xs) = (Brackets expr : zs, ws)
  where
    (expr, ys) = genGroup xs
    (zs, ws) = genGroup ys
genGroup (CloseBrace : xs) = ([], xs)
genGroup (OpenBrace : xs) = (Braces expr : zs, ws)
  where
    (expr, ys) = genGroup xs
    (zs, ws) = genGroup ys
genGroup (ClosePar : xs) = ([], xs)
genGroup (OpenPar : xs) = (Parenthesis expr : zs, ws)
  where
    (expr, ys) = genGroup xs
    (zs, ws) = genGroup ys
genGroup (x : xs) = (A x : ys, zs)
  where
    (ys, zs) = genGroup xs

safeGenGroup :: [Token] -> Maybe ([Expr], [Token])
safeGenGroup [] = Just ([], [])
safeGenGroup (CloseBracket : xs) = Just ([], xs)
safeGenGroup (OpenBracket : xs) = do
  (expr, ys) <- safeGenGroup xs
  (zs, ws) <- safeGenGroup ys
  pure (Brackets expr : zs, ws)
safeGenGroup (CloseBrace : xs) = Just ([], xs)
safeGenGroup (OpenBrace : xs) = do
  (expr, ys) <- safeGenGroup xs
  (zs, ws) <- safeGenGroup ys
  pure (Braces expr : zs, ws)
safeGenGroup (ClosePar : xs) = Just ([], xs)
safeGenGroup (OpenPar : xs) = do
  (expr, ys) <- safeGenGroup xs
  (zs, ws) <- safeGenGroup ys
  pure (Parenthesis expr : zs, ws)
safeGenGroup (x : xs) = do
  (ys, zs) <- safeGenGroup xs
  pure (A x : ys, zs)
