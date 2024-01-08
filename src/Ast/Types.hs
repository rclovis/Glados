module Ast.Types (Type (..)) where

data Type = Ti8 | Ti16 | Ti32 | Ti64 | Tu8 | Tu16 | Tu32 | Tu64 | Tf32 | Tf64
  deriving (Eq, Ord, Show)
