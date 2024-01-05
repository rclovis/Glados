{-# LANGUAGE InstanceSigs #-}

module BuildBytecode (mainBytecodeTest) where

import Bytecode (Bytecode (..), FloatingPoint (..), IntTypes (..), WordTypes (..), getSizeBytecode)
import Control.Applicative (Alternative (..))
import Control.Monad.State
import Data.Sequence as S
import LexerVm (Variable (..))
import Data.Int
import Data.Word
import Data.Bits

-- to remove : le One's AST -> import AST
data Op = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Le | Ge | And | Or | Not
  deriving (Eq, Ord, Show)

data Type = Ti8 | Ti16 | Ti32 | Ti64 | Tu8 | Tu16 | Tu32 | Tu64 | Tf32 | Tf64 | Tnull
  deriving (Eq, Ord, Show)

correspondingInt :: Int -> Integer -> IntTypes
correspondingInt 1 x = Int8Val (fromIntegral x)
correspondingInt 2 x = Int16Val (fromIntegral x)
correspondingInt 4 x = Int32Val (fromIntegral x)
correspondingInt 8 x = Int64Val (fromIntegral x)
correspondingInt _ _ = error "zbi"

correspondingWord :: Int -> Integer -> WordTypes
correspondingWord 1 x = Word8Val (fromIntegral x)
correspondingWord 2 x = Word16Val (fromIntegral x)
correspondingWord 4 x = Word32Val (fromIntegral x)
correspondingWord 8 x = Word64Val (fromIntegral x)
correspondingWord _ _ = error "zbi"

correspondingFloat :: Int -> Rational -> FloatingPoint
correspondingFloat 4 x = FloatVal (fromRational x)
correspondingFloat 8 x = DoubleVal (fromRational x)


isFloat :: Type -> Bool
isFloat Tf32 = True
isFloat Tf64 = True
isFloat _ = False

isInt :: Type -> Bool
isInt Ti8 = True
isInt Ti16 = True
isInt Ti32 = True
isInt Ti64 = True
isInt _ = False

isUnsigned :: Type -> Bool
isUnsigned Tu8 = True
isUnsigned Tu16 = True
isUnsigned Tu32 = True
isUnsigned Tu64 = True
isUnsigned _ = False

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
  { memVar :: S.Seq (S.Seq (String, Type)),
    memFunk :: S.Seq (String, S.Seq (String, Type)),
    bytecode :: S.Seq Bytecode
  }
  deriving (Show, Eq)

emptyMemory :: Memory
emptyMemory = Memory {memVar = S.empty, memFunk = S.empty, bytecode = S.empty}

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

testAst :: Ast
testAst =
  Seq
    [ Assign
        "factorial"
        ( Lambda
            [("n", Ti32), ("b", Tf64)]
            ( Seq
                [ If
                    (BinOp Eq (Var "n") (Int 0))
                    (Seq [Int 1])
                    (Seq []),
                  Seq
                    [ BinOp Mul (Var "n") (Call "factorial" [BinOp Sub (Var "n") (Int 1)])
                    ]
                ]
            )
        ),
      Call "factorial" [Int 5]
    ]

argToSeq :: [Arg] -> S.Seq (String, Type)
argToSeq [] = S.empty
argToSeq ((name, type_) : xs) = (name, type_) <| argToSeq xs


memoryPushVarScope :: MemoryState ()
memoryPushVarScope = MemoryState $ do
  stock <- get
  put stock {memVar = S.empty <| memVar stock}

memoryPopVarScope :: MemoryState ()
memoryPopVarScope = MemoryState $ do
  stock <- get
  put stock {memVar = S.drop 1 (memVar stock)}

memoryPushVar :: String -> Type -> MemoryState ()
memoryPushVar name type_ = MemoryState $ do
  stock <- get
  case memVar stock of
    (memVarTop :<| xs) -> do
      put stock {memVar = (memVarTop |> (name, type_)) <| xs}
    _ -> return ()

memorySetIndexBytecode :: Int -> Bytecode -> MemoryState ()
memorySetIndexBytecode i bc = MemoryState $ do
  stock <- get
  put stock {bytecode = S.update i bc (bytecode stock)}

memoryGetIndexBytecode :: Int -> MemoryState Bytecode
memoryGetIndexBytecode i = MemoryState $ do
  stock <- get
  return $ S.index (bytecode stock) i

memoryGetSizeLastBytecode :: Int -> MemoryState Int
memoryGetSizeLastBytecode 0 = return 0
memoryGetSizeLastBytecode i = MemoryState $ do
  stock <- get
  x <- runMemoryState (memoryGetIndexBytecode (S.length (bytecode stock) - i))
  xs <- runMemoryState (memoryGetSizeLastBytecode (i - 1))
  return (getSizeBytecode x + xs)

encodeArgs :: S.Seq (String, Type) -> Int -> MemoryState ()
encodeArgs S.Empty _ = return ()
encodeArgs ((name, t) :<| xs) g = MemoryState $ do
  stock <- get
  if isInt t then
    put stock {bytecode = bytecode stock |> IloadStack 2 (correspondingInt 2 (toInteger (S.length xs))) |> Istore 2 (correspondingInt 2 (toInteger g))}
  else if isUnsigned t then
    put stock {bytecode = bytecode stock |> UloadStack 2 (correspondingInt 2 (toInteger (S.length xs))) |> Ustore 2 (correspondingInt 2 (toInteger g))}
  else
    put stock {bytecode = bytecode stock |> FloadStack 2 (correspondingInt 2 (toInteger (S.length xs))) |> Fstore 2 (correspondingInt 2 (toInteger g))}
  runMemoryState (memoryPushVar name t)
  runMemoryState (encodeArgs xs (g + 1))



getFunk :: S.Seq (String, Type) -> Ast -> MemoryState ()
getFunk args ast = MemoryState $ do
  stock <- get
  runMemoryState (encodeArgs args 0)
  runMemoryState (getAll ast)


memoryAddFunk :: String -> S.Seq (String, Type) -> MemoryState ()
memoryAddFunk name args = MemoryState $ do
  stock <- get
  put stock {memFunk = memFunk stock |> (name, args)}

getAssign :: Ast -> MemoryState ()
getAssign (Assign name (Lambda args body)) = MemoryState $ do
  stock <- get
  runMemoryState (memoryAddFunk name (argToSeq args))
  put stock {bytecode = bytecode stock |> Funk 4 0}
  i <- gets (S.length . bytecode)
  runMemoryState (getFunk (argToSeq args) body)
  stock <- get
  funkSize <- runMemoryState (memoryGetSizeLastBytecode (S.length (bytecode stock) - i + 1))
  runMemoryState (memorySetIndexBytecode (i - 1) (Funk 4 (fromIntegral funkSize)))

getAssign _ = MemoryState $ do
  stock <- get
  put stock {bytecode = bytecode stock}

getSeq :: Ast -> MemoryState ()
getSeq (Seq []) = return ()
getSeq (Seq (x:xs)) = MemoryState $ do
  runMemoryState (getAll x)
  runMemoryState (getSeq (Seq xs))
getSeq _ = return ()

getAll :: Ast -> MemoryState ()
getAll (Seq asts) = MemoryState $ do
  runMemoryState (getSeq (Seq asts))

getAll (Assign name (Lambda args body)) = MemoryState $ do
  runMemoryState (getAssign (Assign name (Lambda args body)))

getAll _ = return ()


mainBytecodeTest :: IO ()
mainBytecodeTest = do
  let stock = execState (runMemoryState (getAll testAst)) emptyMemory
  print stock


