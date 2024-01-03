{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ast
  ( Ast (..),
    Op (..),
    Type (..),
    Ast.Num (..),
    genAst,
  )
where

import Data.Int
import Data.Word
import Lexer (Token (..))

data Op = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Le | Ge | And | Or | Not
  deriving (Eq, Ord, Show)

data Type = Ti8 | Ti16 | Ti32 | Ti64 | Tu8 | Tu16 | Tu32 | Tu64 | Tf32 | Tf64 | Tbool | Tstr | Tnull
  deriving (Eq, Ord, Show)

data Num
  = I8 Int8
  | I16 Int16
  | I32 Int32
  | I64 Int64
  | U8 Word8
  | U16 Word16
  | U32 Word32
  | U64 Word64
  | F32 Float
  | F64 Double
  deriving (Eq, Ord, Show)

data Ast
  = Seq [Ast]
  | Print Ast
  | Define String Type Ast
  | Lambda [String] Ast
  | Assign String Ast
  | If Ast Ast Ast
  | While Ast Ast
  | Break
  | BinOp Op Ast Ast
  | UnOp Op Ast
  | Id String
  | Num Ast.Num
  | Bool Bool
  | Str String
  | Null
  deriving (Eq, Ord, Show)

genAst :: [Token] -> Maybe [Ast]
genAst = undefined
