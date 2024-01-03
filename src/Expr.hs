{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Expr
  ( Expr (..),
    genExpr,
  )
where

import Lexer (Token (..))

data Expr
  = Parenthesis [Expr]
  | Braces [Expr]
  | Brackets [Expr]
  | Atom Token
  deriving (Eq, Show)

genExpr :: [Token] -> Maybe [Expr]
genExpr [] = Nothing
genExpr tokens = Just expr
  where
    (expr, _) = genGroup tokens

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
genGroup (x : xs) = (Atom x : ys, zs)
  where
    (ys, zs) = genGroup xs
