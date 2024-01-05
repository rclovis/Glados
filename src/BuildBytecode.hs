{-# LANGUAGE InstanceSigs #-}

module BuildBytecode where

import Control.Applicative (Alternative (..))
import Control.Monad.State
import Data.Sequence as S

import Bytecode (Bytecode (..), IntTypes (..), FloatingPoint (..), WordTypes (..))

import LexerVm (Variable (..))

-- to remove : le One's AST -> import AST
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
-- end to remove

data Memory = Memory
    {
        memVar :: S.Seq (S.Seq (String, Variable)),
        memFunk :: S.Seq (String, [(String, Variable)])
    }
    deriving (Show, Eq)

newtype MemoryState a = MemoryState
    { runMemoryState :: State Memory a
    }

instance Functor MemoryState where
    fmap :: (a -> b) -> MemoryState a -> MemoryState b
    fmap f (MemoryState mem) = MemoryState $ do
        a <- get
        let (x, a') = runState mem a
        put a'
        return $ f x

instance Applicative MemoryState where
    pure :: a -> MemoryState a
    pure x = MemoryState $ do
        return x

    (<*>) :: MemoryState (a -> b) -> MemoryState a -> MemoryState b
    (MemoryState mem1) <*> (MemoryState mem2) = MemoryState $ do
        stock <- get
        let (f, stock') = runState mem1 stock
        let (x, stock'') = runState mem2 stock'
        put stock''
        return $ f x

instance Alternative MemoryState where
    empty :: MemoryState a
    empty = MemoryState $ do
        stock <- get
        return undefined

    (<|>) :: MemoryState a -> MemoryState a -> MemoryState a
    (MemoryState mem1) <|> (MemoryState mem2) = MemoryState $ do
        stock <- get
        let (x, stock') = runState mem1 stock
        put stock'
        return x
    
instance Monad MemoryState where
    (>>=) :: MemoryState a -> (a -> MemoryState b) -> MemoryState b
    (MemoryState mem) >>= f = MemoryState $ do
        stock <- get
        let (x, stock') = runState mem stock
        let (MemoryState mem') = f x
        let (y, stock'') = runState mem' stock'
        put stock''
        return y



-- astToBytecode :: Ast -> [Maybe Bytecode]
-- astToBytecode ast =
--         getSeq ast <|>
--         getPrint ast <|>
--         getDefine ast <|>
--         getLambda  ast <|>
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