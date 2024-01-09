module Ast.Op (Op (..)) where

-- | Operators
data Op
  = Add -- Addition
  | Sub -- Subtraction
  | Mul -- Multiplication
  | Div -- Division
  | Mod -- Modulo
  | Eq -- Equal
  | Neq -- Not equal
  | Lt -- Less than
  | Gt -- Greater than
  | Le -- Less than or equal
  | Ge -- Greater than or equal
  | And -- And
  | Or -- Or
  | Not -- Not
  deriving (Eq, Ord, Show)
