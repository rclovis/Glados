{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BuildBytecode (astToBytecode) where

import Ast.Ast (Arg, Ast (..))
import Ast.Op (Op (..))
import Ast.Types (Type (..))
import Bytecode (Bytecode (..), FloatingPoint (..), IntTypes (..), WordTypes (..), getSizeBytecode)
import Control.Applicative (Alternative (..))
import Control.Monad.State
import Data.Bits
import Data.Int
import Data.Map as M
import Data.Sequence as S
import Data.Word
import LexerVm (OpCode (Iconvert, Invoke), Variable (..))

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
correspondingFloat _ _ = error "zbi"

minimumSizeI :: Int -> Int
minimumSizeI x
  | xabs < 128 = 1
  | xabs < 32768 = 2
  | xabs < 2147483648 = 4
  | otherwise = 8
  where
    xabs = abs x

minimumSizeU :: Int -> Int
minimumSizeU x
  | x < 0 = error "zbi"
  | x < 256 = 1
  | x < 65536 = 2
  | x < 4294967296 = 4
  | otherwise = 8

minimumSizeF :: Rational -> Int
minimumSizeF _ = 8

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

data Memory = Memory
  { memVar :: S.Seq (S.Seq (String, Type)),
    memFunk :: S.Seq (String, S.Seq (String, Type)),
    bytecode :: S.Seq Bytecode
  }
  deriving (Show, Eq)

emptyMemory :: Memory
emptyMemory = Memory {memVar = S.singleton S.empty, memFunk = S.empty, bytecode = S.empty}

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

argToSeq :: [Arg] -> S.Seq (String, Type)
argToSeq [] = S.empty
argToSeq ((name, type_) : xs) = (name, type_) <| argToSeq xs

memoryGetFunk :: String -> Seq (String, Seq (String, Type)) -> MemoryState (Seq (String, Type))
memoryGetFunk name S.Empty = MemoryState $ do
  stock <- get
  return S.Empty
memoryGetFunk name ((nameFunk, args) :<| xs) = MemoryState $ do
  if name == nameFunk
    then return args
    else runMemoryState (memoryGetFunk name xs)

memoryGetFunkIndex :: String -> Seq (String, Seq (String, Type)) -> Int
memoryGetFunkIndex _ S.Empty = error "Empty"
memoryGetFunkIndex name ((nameFunk, _) :<| xs) =
  if name == nameFunk
    then 0
    else 1 + memoryGetFunkIndex name xs

memoryGetVarIndex :: String -> Seq (String, Type) -> Int
memoryGetVarIndex _ S.Empty = error "Empty"
memoryGetVarIndex name ((nameVar, _) :<| xs) =
  if name == nameVar
    then 0
    else 1 + memoryGetVarIndex name xs

memoryGetVar :: String -> Seq (String, Type) -> MemoryState (String, Type)
memoryGetVar _ S.Empty = error "Empty"
memoryGetVar name ((nameVar, type_) :<| xs) =
  if name == nameVar
    then return (nameVar, type_)
    else memoryGetVar name xs

memoryPushVarScope :: MemoryState ()
memoryPushVarScope = MemoryState $ do
  stock <- get
  put stock {memVar = S.empty <| memVar stock}

memoryPopVarScope :: MemoryState ()
memoryPopVarScope = MemoryState $ do
  stock <- get
  put stock {memVar = S.drop 1 (memVar stock)}

memoryGetTopScope :: MemoryState (S.Seq (String, Type))
memoryGetTopScope = MemoryState $ do
  stock <- get
  return (S.index (memVar stock) 0)

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

memoryGetSizeBytecodeXtoY :: Int -> Int -> MemoryState Int
memoryGetSizeBytecodeXtoY x y
  | x > y = error "bad index"
  | x == y = return 0
memoryGetSizeBytecodeXtoY x y = MemoryState $ do
  size1 <- runMemoryState (memoryGetIndexBytecode x)
  if (x + 1) == y
    then return (getSizeBytecode size1)
    else do
      size2 <- runMemoryState (memoryGetSizeBytecodeXtoY (x + 1) y)
      return (getSizeBytecode size1 + size2)

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
  runMemoryState (memoryPushVar name t)
  if isInt t
    then put stock {bytecode = bytecode stock |> IloadStack 2 (correspondingInt 2 (toInteger (S.length xs))) |> Istore 2 (correspondingInt 2 (toInteger g))}
    else
      if isUnsigned t
        then put stock {bytecode = bytecode stock |> UloadStack 2 (correspondingInt 2 (toInteger (S.length xs))) |> Ustore 2 (correspondingInt 2 (toInteger g))}
        else put stock {bytecode = bytecode stock |> FloadStack 2 (correspondingInt 2 (toInteger (S.length xs))) |> Fstore 2 (correspondingInt 2 (toInteger g))}
  runMemoryState (memoryPushVar name t)
  runMemoryState (encodeArgs xs (g + 1))

getFunk :: S.Seq (String, Type) -> Ast -> MemoryState ()
getFunk args ast = MemoryState $ do
  runMemoryState memoryPushVarScope
  runMemoryState (encodeArgs args 0)
  runMemoryState (getAll ast)
  runMemoryState memoryPopVarScope

memoryAddFunk :: String -> S.Seq (String, Type) -> MemoryState ()
memoryAddFunk name args = MemoryState $ do
  stock <- get
  put stock {memFunk = memFunk stock |> (name, args)}

getConvert :: Type -> MemoryState ()
getConvert t = MemoryState $ do
  stock <- get
  case t of
    Ti8 -> put stock {bytecode = bytecode stock |> Bytecode.Iconvert 1 1}
    Ti16 -> put stock {bytecode = bytecode stock |> Bytecode.Iconvert 1 2}
    Ti32 -> put stock {bytecode = bytecode stock |> Bytecode.Iconvert 1 4}
    Ti64 -> put stock {bytecode = bytecode stock |> Bytecode.Iconvert 1 8}
    Tu8 -> put stock {bytecode = bytecode stock |> Bytecode.Uconvert 1 1}
    Tu16 -> put stock {bytecode = bytecode stock |> Bytecode.Uconvert 1 2}
    Tu32 -> put stock {bytecode = bytecode stock |> Bytecode.Uconvert 1 4}
    Tu64 -> put stock {bytecode = bytecode stock |> Bytecode.Uconvert 1 8}
    Tf32 -> put stock {bytecode = bytecode stock |> Bytecode.Fconvert 1 4}
    Tf64 -> put stock {bytecode = bytecode stock |> Bytecode.Fconvert 1 8}

getDefine :: Ast -> MemoryState ()
getDefine (Define name _ (Lambda args body)) = MemoryState $ do
  runMemoryState (memoryAddFunk name (argToSeq args))
  stock1 <- get
  put stock1 {bytecode = bytecode stock1 |> Funk 4 (correspondingInt 4 0)}
  i <- gets (S.length . bytecode)
  runMemoryState (getFunk (argToSeq args) body)
  stock2 <- get
  funkSize <- runMemoryState (memoryGetSizeLastBytecode (S.length (bytecode stock2) - i + 1))
  runMemoryState (memorySetIndexBytecode (i - 1) (Funk 4 (correspondingInt 4 (toInteger funkSize))))
getDefine (Define name t ast) = MemoryState $ do
  runMemoryState (memoryPushVar name t)
  runMemoryState (getAll ast)
  runMemoryState (getConvert t)
  stock <- get

  if isInt t
    then put stock {bytecode = bytecode stock |> Istore 2 (correspondingInt 2 (toInteger (memoryGetVarIndex name (index (memVar stock) 0))))}
    else
      if isUnsigned t
        then put stock {bytecode = bytecode stock |> Ustore 2 (correspondingInt 2 (toInteger (memoryGetVarIndex name (index (memVar stock) 0))))}
        else put stock {bytecode = bytecode stock |> Fstore 2 (correspondingInt 2 (toInteger (memoryGetVarIndex name (index (memVar stock) 0))))}
getDefine _ = return ()

getAssign :: Ast -> MemoryState ()
getAssign (Assign name ast) = MemoryState $ do
  runMemoryState (getAll ast)
  stock1 <- get
  var <- runMemoryState (memoryGetVar name (index (memVar stock1) 0))
  runMemoryState (getConvert (snd var))
  stock2 <- get
  if isInt (snd var)
    then put stock2 {bytecode = bytecode stock2 |> Istore 2 (correspondingInt 2 (toInteger (memoryGetVarIndex name (index (memVar stock2) 0))))}
    else
      if isUnsigned (snd var)
        then put stock2 {bytecode = bytecode stock2 |> Ustore 2 (correspondingInt 2 (toInteger (memoryGetVarIndex name (index (memVar stock2) 0))))}
        else put stock2 {bytecode = bytecode stock2 |> Fstore 2 (correspondingInt 2 (toInteger (memoryGetVarIndex name (index (memVar stock2) 0))))}
getAssign _ = MemoryState $ do
  stock <- get
  put stock {bytecode = bytecode stock}

getSeq :: Ast -> MemoryState ()
getSeq (Seq []) = return ()
getSeq (Seq (x : xs)) = MemoryState $ do
  runMemoryState (getAll x)
  runMemoryState (getSeq (Seq xs))
getSeq _ = return ()

pushArgs :: [Ast] -> Seq (String, Type) -> MemoryState ()
pushArgs [] _ = return ()
pushArgs (x : xs) ((_, t) :<| ys) = MemoryState $ do
  runMemoryState (getAll x)
  runMemoryState (getConvert t)
  runMemoryState (pushArgs xs ys)
pushArgs _ _ = return ()

getCall :: Ast -> MemoryState ()
getCall (Call name args) = MemoryState $ do
  stock1 <- get
  funk <- runMemoryState (memoryGetFunk name (memFunk stock1))
  runMemoryState (pushArgs args funk)
  -- runMemoryState (pushArgs args funk)
  stock2 <- get
  put
    stock2
      { bytecode =
          bytecode stock2
            |> Bytecode.Invoke 2 (correspondingInt 2 (toInteger (memoryGetFunkIndex name (memFunk stock2))))
            |> Bytecode.PopPrev 2 (correspondingInt 2 (toInteger (S.length funk)))
      }
getCall _ = return ()

getInt :: Ast -> MemoryState ()
getInt (Int n) = MemoryState $ do
  stock <- get
  put stock {bytecode = bytecode stock |> Iconst (fromIntegral (minimumSizeI n) :: Word8) (correspondingInt (minimumSizeI n) (toInteger n))}
getInt _ = return ()

getFloat :: Ast -> MemoryState ()
getFloat (Float n) = MemoryState $ do
  stock <- get
  put stock {bytecode = bytecode stock |> Fconst (fromIntegral (minimumSizeF (toRational n)) :: Word8) (correspondingFloat (minimumSizeF (toRational n)) (toRational n))}
getFloat _ = return ()

getIf :: Ast -> MemoryState ()
getIf (If cond body1 body2) = MemoryState $ do
  runMemoryState (getAll cond)
  stock1 <- get
  let save1 = S.length (bytecode stock1)
  runMemoryState (getAll body2)
  stock2 <- get
  let goto = S.length (bytecode stock2)
  runMemoryState (getAll body1)
  stock4 <- get
  sizeofBytecode2 <- runMemoryState (memoryGetSizeBytecodeXtoY goto (S.length (bytecode stock4)))
  put stock4 {bytecode = S.take goto (bytecode stock4) <> S.singleton (Goto 2 (correspondingInt 2 (toInteger (sizeofBytecode2 + 4)))) <> S.drop goto (bytecode stock4)}
  stock5 <- get
  sizeofBytecode <- runMemoryState (memoryGetSizeBytecodeXtoY save1 (S.length (bytecode stock2) + 1))
  put stock5 {bytecode = S.take save1 (bytecode stock5) <> S.singleton (Ift 2 (correspondingInt 2 (toInteger (sizeofBytecode + 4)))) <> S.drop save1 (bytecode stock5)}
getIf _ = return ()

getUnOp :: Ast -> MemoryState ()
getUnOp (UnOp op ast) = MemoryState $ do
  runMemoryState (getAll ast)
  stock <- get
  case op of
    Ast.Op.Not -> put stock {bytecode = bytecode stock |> Bytecode.Not}
    _ -> error "not supported Unary Operator"
getUnOp _ = return ()

getBinOp :: Ast -> MemoryState ()
getBinOp (BinOp op ast1 ast2) = MemoryState $ do
  runMemoryState (getAll ast1)
  runMemoryState (getAll ast2)
  stock <- get
  case op of
    Add -> put stock {bytecode = bytecode stock |> Iadd}
    Sub -> put stock {bytecode = bytecode stock |> Isub}
    Mul -> put stock {bytecode = bytecode stock |> Imul}
    Div -> put stock {bytecode = bytecode stock |> Idiv}
    Mod -> put stock {bytecode = bytecode stock |> Imod}
    Eq -> put stock {bytecode = bytecode stock |> Ieq}
    Neq -> put stock {bytecode = bytecode stock |> Ine}
    Lt -> put stock {bytecode = bytecode stock |> Ilt}
    Gt -> put stock {bytecode = bytecode stock |> Igt}
    Le -> put stock {bytecode = bytecode stock |> Ile}
    Ge -> put stock {bytecode = bytecode stock |> Ige}
    Ast.Op.And -> put stock {bytecode = bytecode stock |> Iand}
    Or -> put stock {bytecode = bytecode stock |> Bytecode.Ior}
    -- Not -> put stock {bytecode = bytecode stock |> Inot}
    _ -> error "not supported Binary Operator"
getBinOp _ = return ()

getWhile :: Ast -> MemoryState ()
getWhile (While cond body) = MemoryState $ do
  stock <- get
  let save1 = S.length (bytecode stock)
  runMemoryState (getAll body)
  runMemoryState (getAll cond)
  stock2 <- get
  sizeofBytecode <- runMemoryState (memoryGetSizeBytecodeXtoY save1 (S.length (bytecode stock2)))
  put stock2 {bytecode = bytecode stock2 |> Bytecode.Ift 2 (correspondingInt 2 (-1 * toInteger (sizeofBytecode + 4)))}
getWhile _ = return ()

getId :: Ast -> MemoryState ()
getId (Id name) = MemoryState $ do
  stock <- get
  put stock {bytecode = bytecode stock |> Iload 2 (correspondingInt 2 (toInteger (memoryGetVarIndex name (index (memVar stock) 0))))}
getId _ = return ()

getReturn :: Ast -> MemoryState ()
getReturn (Ast.Ast.Return ast) = MemoryState $ do
  runMemoryState (getAll ast)
  stock <- get
  put stock {bytecode = bytecode stock |> Bytecode.Return}
getReturn _ = return ()

getArrayValue :: Ast -> Type -> MemoryState ()
getArrayValue (ArrayValue (a : as)) t = MemoryState $ do
  stock <- get
  case (t, a) of
    (Ti8, Int n) -> put stock {bytecode = bytecode stock |> Bytecode.Iconst 1 (correspondingInt 1 (toInteger n))}
    (Ti16, Int n) -> put stock {bytecode = bytecode stock |> Bytecode.Iconst 2 (correspondingInt 2 (toInteger n))}
    (Ti32, Int n) -> put stock {bytecode = bytecode stock |> Bytecode.Iconst 4 (correspondingInt 4 (toInteger n))}
    (Ti64, Int n) -> put stock {bytecode = bytecode stock |> Bytecode.Iconst 8 (correspondingInt 8 (toInteger n))}
    (Tu8, Int n) -> put stock {bytecode = bytecode stock |> Bytecode.Uconst 1 (correspondingWord 1 (toInteger n))}
    (Tu16, Int n) -> put stock {bytecode = bytecode stock |> Bytecode.Uconst 2 (correspondingWord 2 (toInteger n))}
    (Tu32, Int n) -> put stock {bytecode = bytecode stock |> Bytecode.Uconst 4 (correspondingWord 4 (toInteger n))}
    (Tu64, Int n) -> put stock {bytecode = bytecode stock |> Bytecode.Uconst 8 (correspondingWord 8 (toInteger n))}
    (Tf32, Float n) -> put stock {bytecode = bytecode stock |> Bytecode.Fconst 4 (correspondingFloat 4 (toRational n))}
    (Tf64, Float n) -> put stock {bytecode = bytecode stock |> Bytecode.Fconst 8 (correspondingFloat 8 (toRational n))}
    _ -> error "zbi"
  runMemoryState (getArrayValue (ArrayValue as) t)
getArrayValue _ _ = return ()

getArray :: Ast -> MemoryState ()
getArray (Array types sizes arrayVal) = MemoryState $ do
  runMemoryState (getArrayValue arrayVal types)
  stock <- get
  put stock {bytecode = bytecode stock |> Bytecode.Addr 8 (correspondingWord 8 (toInteger sizes))}
getArray _ = return ()

getIndexing :: Ast -> MemoryState ()
getIndexing (Indexing name ast) = MemoryState $ do
  runMemoryState (getAll ast)
  stock <- get
  put stock {bytecode = bytecode stock |> Iload 2 (correspondingInt 2 (toInteger (memoryGetVarIndex name (index (memVar stock) 0))))}
  stock2 <- get
  put stock2 {bytecode = bytecode stock2 |> Bytecode.Iadd |> Bytecode.Access}
getIndexing _ = return ()

getAssignArray :: Ast -> MemoryState ()
getAssignArray (AssignArray str ast1 ast2) = MemoryState $ do
  runMemoryState (getAll ast1)
  stock <- get
  put stock {bytecode = bytecode stock |> Iload 2 (correspondingInt 2 (toInteger (memoryGetVarIndex str (index (memVar stock) 0)))) |> Bytecode.Iadd}
  runMemoryState (getAll ast2)
  stock2 <- get
  put stock2 {bytecode = bytecode stock2 |> Bytecode.Modify}
getAssignArray _ = return ()

getAll :: Ast -> MemoryState ()
getAll (Seq asts) = MemoryState $ do
  runMemoryState (getSeq (Seq asts))
getAll (Assign name ast) = MemoryState $ do
  runMemoryState (getAssign (Assign name ast))
getAll (Call name args) = MemoryState $ do
  runMemoryState (getCall (Call name args))
getAll (Int n) = MemoryState $ do
  runMemoryState (getInt (Int n))
getAll (Float n) = MemoryState $ do
  runMemoryState (getFloat (Float n))
getAll (BinOp op ast1 ast2) = MemoryState $ do
  runMemoryState (getBinOp (BinOp op ast1 ast2))
getAll (If cond body1 body2) = MemoryState $ do
  runMemoryState (getIf (If cond body1 body2))
getAll (Define name type_ ast) = MemoryState $ do
  runMemoryState (getDefine (Define name type_ ast))
getAll (Id name) = MemoryState $ do
  runMemoryState (getId (Id name))
getAll (Ast.Ast.Return ast) = MemoryState $ do
  runMemoryState (getReturn (Ast.Ast.Return ast))
getAll (While cond body) = MemoryState $ do
  runMemoryState (getWhile (While cond body))
getAll (UnOp op ast) = MemoryState $ do
  runMemoryState (getUnOp (UnOp op ast))
getAll (Array type_ siz ast) = MemoryState $ do
  runMemoryState (getArray (Array type_ siz ast))
getAll (Indexing name ast) = MemoryState $ do
  runMemoryState (getIndexing (Indexing name ast))
getAll (AssignArray str ast1 ast2) = MemoryState $ do
  runMemoryState (getAssignArray (AssignArray str ast1 ast2))
getAll _ = return ()

bcSecToList :: S.Seq Bytecode -> [Bytecode]
bcSecToList S.Empty = []
bcSecToList (x :<| xs) = x : bcSecToList xs

astToBytecode :: Ast -> [Bytecode]
astToBytecode ast = do
  let stock = execState (runMemoryState (getAll ast)) emptyMemory
  bcSecToList (bytecode stock)
