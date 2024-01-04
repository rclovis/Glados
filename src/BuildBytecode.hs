module BuildBytecode where

import Control.Applicative

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)

data IntTypes
  = Int8Val Int8
  | Int16Val Int16
  | Int32Val Int32
  | Int64Val Int64
  deriving (Show, Eq)

data WordTypes
  = Word8Val Word8
  | Word16Val Word16
  | Word32Val Word32
  | Word64Val Word64
  deriving (Show, Eq)

data FloatingPoint
  = FloatVal Float
  | DoubleVal Double
  deriving (Show, Eq)

data Bytecode
  = Funk Word8 Word8 -- function with int size of arg and int size of body
  | Iload Word8 Word8 -- load int from stack
  | Fload Word8 Word8 -- load float from stack
  | Uload Word8 Word8 -- load uint from stack
  | Istore Word8 Word8 -- store int to stack
  | Fstore Word8 Word8 -- store float to stacktraverse toBin xs
  | Ustore Word8 Word8 -- store uint to stack
  | Iconst Word8 IntTypes -- push int to stack
  | Fconst Word8 FloatingPoint -- push float to stack
  | Uconst Word8 WordTypes -- push uint to stack
  | Iadd -- add int or uint
  | Fadd -- add float
  | Isub -- subtract int or uint
  | Fsub -- subtract float
  | Imul -- multiply int or uint
  | Fmul -- multiply float
  | Idiv -- divide int or uint
  | Fdiv -- divide float
  | Imod -- modulo int or uintù
  | Ieq -- equal int or uint
  | Feq -- equal float
  | Ineq -- not equal int or uint
  | Fneq -- not equal float
  | Igt -- greater than int or uint
  | Fgt -- greater than float
  | Ilt -- less than int or uint
  | Flt -- less than float
  | Ige -- greater than or equal int or uint
  | Fge -- greater than or equal float
  | Ile -- less than or equal int or uint
  | Fle -- less than or equal float
  | Iand -- and int or uint
  | Ior -- or int or uint
  | Ixor -- xor int or uint
  | Ift Word8 Int -- if true jump of int bytes
  | Iff Word8 Int -- if false jump of int bytes
  | Goto Word8 Int -- jump of int bytes
  | Invoke Word8 Int -- invoke function at int index
  | Return -- return from function
  | I2f -- convert int to float
  | F2i -- convert float to int
  | Consume Word8 Word8 -- consume Int args of the stack 
  | NOTHING -- do nothing

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

astToBytecode :: Ast -> [Maybe Bytecode]
astToBytecode ast =
        getSeq ast <|>
        getPrint ast <|>
        getDefine ast <|>
        getLambda ast <|>
        getCall ast <|>
        getAssign ast <|>
        getIf ast <|>
        getWhile ast <|>
        getBreak ast <|>
        getBinOp ast <|>
        getUnOp ast <|>
        getId ast <|>
        getInt ast <|>
        getFloat ast <|>
        getBool ast <|>
        getStr ast <|>
        getVar ast <|>
        getNull ast <|>
        error "zbi"

getSeq :: Ast -> [Maybe Bytecode]
getSeq (Seq (ast : rest)) = astToBytecode ast ++ getSeq (Seq rest)
getSeq _ = []

getPrint :: Ast -> [Maybe Bytecode]
getPrint (Print ast) = astToBytecode ast
getPrint _ = []

getLambda :: Ast -> [Maybe Bytecode]
getLambda (Lambda args ast) = 



-- Skipped 
getDefine :: Ast -> [Maybe Bytecode]
getDefine (Define {}) = []
getDefine _ = []


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

expected :: [Bytecode]
expected =
    [
        Funk 1 0,
        Iload 8 0,
        Iconst 8 (Int64Val 0), 
        Ieq,
        Iff 1 14,
        Iconst 8 (Int64Val 1),
        Return,
        Iload 8 0,
        Iload 8 0,
        Iconst 8 (Int64Val 1),
        Isub,
        Invoke 1 0,
        Imul,
        Return,

        Iconst 8 (Int64Val 5),
        Invoke 1 0,
    ]

fracTest :: Ast
fracTest = 
    BinOp Div (Int 1) (Int 2)
