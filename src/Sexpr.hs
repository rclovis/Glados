{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Sexpr
  ( Sexpr (..),
    parseSexpr,
  )
where

import Lexer (Token (..))

data Sexpr
  = INum Int
  | FNum Float
  | Str String
  | Bool Bool
  | Sym String
  | List [Sexpr]
  deriving (Show)

parseSexpr :: Maybe [Token] -> [Sexpr]
parseSexpr Nothing = error "Nothing"
parseSexpr (Just tokens) =
  let (sexpr, remainingsTokens) = parseExpr tokens
   in if null remainingsTokens
        then [sexpr]
        else sexpr : parseSexpr (Just remainingsTokens)

parseExpr :: [Token] -> (Sexpr, [Token])
parseExpr (OpenPar : restTokens) = parseListSexpr restTokens []
parseExpr ((Symbol a) : restTokens) = (Sym a, restTokens)
parseExpr ((String a) : restTokens) = (Str a, restTokens)
parseExpr ((Boolean a) : restTokens) = (Bool a, restTokens)
parseExpr ((INumber a) : restTokens) = (INum a, restTokens)
parseExpr ((FNumber a) : restTokens) = (FNum a, restTokens)
parseExpr (ClosePar : _) = error "Unexpected ClosePar encountered"
parseExpr (Null : _) = error "Unexpected Null encountered"
parseExpr [] = error "Invalid expression"

parseListSexpr :: [Token] -> [Sexpr] -> (Sexpr, [Token])
parseListSexpr [] _ = error "Mismatched paranthesis"
parseListSexpr (ClosePar : restTokens) acc = (List (reverse acc), restTokens)
parseListSexpr tokens acc =
  let (token, restTokens) = parseExpr tokens
   in parseListSexpr restTokens (token : acc)
