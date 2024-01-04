module BuildBytecode where

import Control.Applicative (Alternative (..))
import Control.Monad.State
import Data.Sequence as S

import Bytecode (Bytecode (..), IntTypes (..), FloatingPoint (..), WordTypes (..))

data Op = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Le | Ge | And | Or | Not
  deriving (Eq, Ord, Show)

data Type = Ti8 | Ti16 | Ti32 | Ti64 | Tu8 | Tu16 | Tu32 | Tu64 | Tf32 | Tf64 | Tbool | Tstr | Tnull
  deriving (Eq, Ord, Show)

type Arg = (String, Type)

data Ast
  = Seq [Ast]
  | Print Ast
  | Define String Type Ast
  | Lambda [Arg] Ast
  | Call String [Ast]
  | Assign String Ast
  | If Ast Ast Ast
  | While Ast Ast
  | Break
  | BinOp Op Ast Ast
  | UnOp Op Ast
  | Id String
  | Int Int
  | Float Float
  | Bool Bool
  | Str String
  | Var String
  | Null
  deriving (Eq, Ord, Show)

-- astToBytecode :: Ast -> [Maybe Bytecode]
-- astToBytecode ast =
--         getSeq ast <|>
--         getPrint ast <|>
--         getDefine ast <|>
--         getLambda ast <|>
--         getCall ast <|>
--         getAssign ast <|>
--         getIf ast <|>
--         getWhile ast <|>
--         getBreak ast <|>
--         getBinOp ast <|>
--         getUnOp ast <|>
--         getId ast <|>
--         getInt ast <|>
--         getFloat ast <|>
--         getBool ast <|>
--         getStr ast <|>
--         getVar ast <|>
--         getNull ast <|>
--         error "zbi"

-- getSeq :: Ast -> [Maybe Bytecode]
-- getSeq (Seq (ast : rest)) = astToBytecode ast ++ getSeq (Seq rest)
-- getSeq _ = []

-- getPrint :: Ast -> [Maybe Bytecode]
-- getPrint (Print ast) = astToBytecode ast
-- getPrint _ = []

-- -- Skipped 
-- getDefine :: Ast -> [Maybe Bytecode]
-- getDefine (Define {}) = []
-- getDefine _ = []


testAst:: Ast
testAst =
    Seq [
        Assign "factorial"
            (Lambda [("n", Ti32)] 
                (Seq [
                    If
                        (BinOp Eq (Var "n") (Int 0))
                        (Seq [Int 1])
                        (Seq []),
                    Seq [
                        BinOp Mul (Var "n") (Call "factorial" [BinOp Sub (Var "n") (Int 1)])
                    ]
                ])
            ),
        Call "factorial" [Int 5]
    ]

-- funk factorial(n: i64): i64 {
--   if (n == 0) {
--     1
--   }
--   n * factorial(n - 1)
-- }
-- factorial(5);