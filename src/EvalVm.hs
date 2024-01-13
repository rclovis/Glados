{-# LANGUAGE InstanceSigs #-}

module EvalVm
  ( mainTest,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.State
import Data.Sequence as S
import Data.Word
import LexerVm
  ( Instruction,
    OpCode (..),
    Variable (..),
  )
import OpNumber
  ( addF,
    addI,
    andI,
    divF,
    divI,
    eqF,
    eqI,
    geF,
    geI,
    gtF,
    gtI,
    leF,
    leI,
    ltF,
    ltI,
    modI,
    mulF,
    mulI,
    neF,
    neI,
    orI,
    subF,
    subI,
    xorI,
  )
import Data.Char (chr)
import System.Exit

data Cpu = Cpu
  { ip :: Int, -- Instruction pointer
    fp :: Int, -- Frame pointer
    cpuStack :: S.Seq Variable, -- The stack
    cpuFunk :: S.Seq Int, -- The function stack
    cpuVar :: S.Seq (S.Seq Variable), -- Variables
    ranOp :: Int, -- The last opcode that was executed
    cpuState :: Int, -- The status of the program
    loop :: Int, -- The loop counter
    heap :: S.Seq Variable, -- The heap
    directory :: S.Seq (Int, Int), -- The directory (heap pointer, size)
    stdOut :: String,
    exitCode :: Int,
    instructions :: S.Seq Instruction,
    args :: S.Seq Variable
  }
  deriving (Show, Eq)

emptyCpu :: Cpu
emptyCpu =
  Cpu
    { ip = 0,
      fp = -1,
      cpuStack = S.empty,
      cpuFunk = S.empty,
      cpuVar = S.singleton S.empty,
      ranOp = 0,
      cpuState = 0,
      loop = 0,
      heap = S.empty,
      directory = S.empty,
      stdOut = "",
      exitCode = 0,
      instructions = S.empty,
      args = S.empty
    }

newtype Operation a = Operation
  { runOperation :: State Cpu a
  }

instance Functor Operation where
  fmap :: (a -> b) -> Operation a -> Operation b
  fmap f (Operation op) = Operation $ do
    cpu <- get
    let (x, cpu') = runState op cpu
    put cpu'
    return $ f x

instance Applicative Operation where
  pure :: a -> Operation a
  pure x = Operation $ do
    return x

  (<*>) :: Operation (a -> b) -> Operation a -> Operation b
  (Operation op1) <*> (Operation op2) = Operation $ do
    cpu <- get
    let (f, cpu') = runState op1 cpu
    let (x, cpu'') = runState op2 cpu'
    put cpu''
    return $ f x

instance Alternative Operation where
  empty :: Operation a
  empty = Operation $ do
    return undefined

  (<|>) :: Operation a -> Operation a -> Operation a
  (Operation op1) <|> (Operation _) = Operation $ do
    cpu <- get
    let (x, cpu') = runState op1 cpu
    put cpu'
    return x

instance Monad Operation where
  (>>=) :: Operation a -> (a -> Operation b) -> Operation b
  (Operation op1) >>= f = Operation $ do
    cpu <- get
    let (x, cpu') = runState op1 cpu
    let (Operation op2) = f x
    let (y, cpu'') = runState op2 cpu'
    put cpu''
    return y

jumpHeap :: Int -> Int -> Int -> S.Seq (Int, Int) -> Int
jumpHeap x size current dir =
  if current >= size + x + 1
    then x
    else
      case jumpOne current dir of
        0 -> jumpHeap x size (current + 1) dir
        x' -> jumpHeap x' size x' dir
      where
        jumpOne :: Int -> S.Seq (Int, Int) -> Int
        jumpOne _ S.Empty = 0
        jumpOne x' (y' :<| ys') =
          if x' >= fst y' && x' < uncurry (+) y'
            then uncurry (+) y'
            else jumpOne x' ys'

operationExtendHeap :: Int -> Operation ()
operationExtendHeap sizeToAdd = Operation $ do
  cpu <- get
  let newHeap = S.replicate sizeToAdd None
  put cpu {heap = newHeap <> heap cpu}

operationGetArg :: Int -> Operation ()
operationGetArg x = Operation $ do
  cpu <- get
  let arg = args cpu `S.index` (S.length (args cpu) - 1 - x)
  runOperation (operationPushStack arg)

operationAddDirectory :: Int -> Int -> Operation ()
operationAddDirectory x y = Operation $ do
  cpu <- get
  let newDirectory = S.singleton (x, y)
  put cpu {directory = directory cpu <> newDirectory}

operationGetHeap :: Int -> Operation ()
operationGetHeap x = Operation $ do
  cpu <- get
  let heapPointer = jumpHeap 0 x 0 (directory cpu)
  if heapPointer + x >= S.length (heap cpu)
    then do
      runOperation (operationExtendHeap (heapPointer + x - S.length (heap cpu)))
      runOperation (operationPushStack (U64 (fromIntegral heapPointer + (2 :: Word64) ^ (63 :: Word64))))
      runOperation (operationAddDirectory heapPointer x)
    else do
      runOperation (operationPushStack (U64 (fromIntegral heapPointer + (2 :: Word64) ^ (63 :: Word64))))
      runOperation (operationAddDirectory heapPointer x)

operationSetIp :: Int -> Operation ()
operationSetIp x = Operation $ do
  cpu <- get
  put cpu {ip = x}

operationSetExitCode :: Int -> Operation ()
operationSetExitCode x = Operation $ do
  cpu <- get
  put cpu {exitCode = x}

operationAddLoop :: Operation ()
operationAddLoop = Operation $ do
  cpu <- get
  put cpu {loop = loop cpu + 1}

operationAddIp :: Operation ()
operationAddIp = Operation $ do
  cpu <- get
  put cpu {ip = ip cpu + 1}

operationSetFp :: Int -> Operation ()
operationSetFp x = Operation $ do
  cpu <- get
  put cpu {fp = x}

operationPushFunk :: Int -> Operation ()
operationPushFunk x = Operation $ do
  cpu <- get
  put cpu {cpuFunk = x <| cpuFunk cpu}

operationPopStack :: Operation Variable
operationPopStack = Operation $ do
  cpu <- get
  case cpuStack cpu of
    S.Empty -> return (I64 0)
    (x :<| xs) -> do
      put cpu {cpuStack = xs}
      return x

operationPopStackDiscard :: Operation ()
operationPopStackDiscard = Operation $ do
  cpu <- get
  put cpu {cpuStack = S.drop 1 (cpuStack cpu)}

operationPopStackIndex :: Int -> Operation Variable
operationPopStackIndex x = Operation $ do
  cpu <- get
  let i = cpuStack cpu `S.index` x
  put cpu {cpuStack = S.deleteAt x (cpuStack cpu)}
  return i

operationGetStackIndex :: Int -> Operation Variable
operationGetStackIndex x = Operation $ do
  cpu <- get
  return $ cpuStack cpu `S.index` x

operationPushStack :: Variable -> Operation ()
operationPushStack x = Operation $ do
  cpu <- get
  put cpu {cpuStack = x <| cpuStack cpu}

operationPushVar :: Variable -> Operation ()
operationPushVar x = Operation $ do
  cpu <- get
  case cpuVar cpu of
    (cpuVarTop :<| xs) -> do
      put cpu {cpuVar = (x :<| cpuVarTop) <| xs}
    _ -> return ()

operationSetVar :: Int -> Variable -> Operation ()
operationSetVar x y = Operation $ do
  cpu <- get
  case cpuVar cpu of
    (cpuVarTop :<| xs) -> do
      if S.length cpuVarTop > x
        then put cpu {cpuVar = S.update (S.length cpuVarTop - 1 - x) y cpuVarTop <| xs}
        else runOperation (operationPushVar y)
    _ -> return ()

operationLoadVar :: Int -> Operation ()
operationLoadVar x = Operation $ do
  cpu <- get
  case cpuVar cpu of
    (cpuVarTop :<| _) -> do
      let i = cpuVarTop `S.index` (S.length cpuVarTop - 1 - x)
      runOperation (operationPushStack i)
    _ -> return ()

operationSetInstruction :: S.Seq Instruction -> Operation ()
operationSetInstruction x = Operation $ do
  cpu <- get
  put cpu {instructions = x}

operationAddStdOut :: Variable -> Operation ()
operationAddStdOut x = Operation $ do
  cpu <- get
  let c = chr (getIntegral x :: Int)
  put cpu {stdOut = stdOut cpu ++ [c]}

operationAddr :: Variable -> Operation ()
operationAddr x = Operation $ do
  cpu <- get
  put cpu {cpuStack = U64 (fromIntegral (S.length (cpuStack cpu) - getIntegral x)) <| cpuStack cpu}

operationAccess :: Word64 -> Operation ()
operationAccess x = Operation $ do
  cpu <- get
  if x >= ((2 :: Word64) ^ (63 :: Word64))
    then do
      let heapPointer = fromIntegral (x - (2 :: Word64) ^ (63 :: Word64))
      put cpu {cpuStack = heap cpu `S.index` (S.length (heap cpu) - 1 - heapPointer) <| cpuStack cpu}
    else do
      put cpu {cpuStack = cpuStack cpu `S.index` (S.length (cpuStack cpu) - 1 - fromIntegral x) <| cpuStack cpu}

operationModify :: Word64 -> Variable -> Operation ()
operationModify x y = Operation $ do
  cpu <- get
  if x >= ((2 :: Word64) ^ (63 :: Word64))
    then do
      let heapPointer = fromIntegral (x - (2 :: Word64) ^ (63 :: Word64))
      put cpu {heap = S.update (S.length (heap cpu) - 1 - heapPointer) y (heap cpu)}
    else do
      put cpu {cpuStack = S.update (S.length (cpuStack cpu) - 1 - fromIntegral x) y (cpuStack cpu)}
  -- put cpu {cpuStack = S.update (S.length (cpuStack cpu) - 1 - x) y (cpuStack cpu)}

operationStoreVar :: Int -> Operation ()
operationStoreVar x = Operation $ do
  pop <- runOperation operationPopStack
  runOperation (operationSetVar x pop)

operationRepeatOp :: Int -> Operation () -> Operation ()
operationRepeatOp 0 _ = return ()
operationRepeatOp x op = do
  op
  operationRepeatOp (x - 1) op

operationJump :: (Integral a) => a -> Operation ()
operationJump x = Operation $ do
  cpu <- get
  runOperation (operationSetIp (ip cpu + fromIntegral x + if x < 0 then 1 else 0))

operationPushVariableScope :: Operation ()
operationPushVariableScope = Operation $ do
  cpu <- get
  put cpu {cpuVar = S.empty <| cpuVar cpu}

operationPopVariableScope :: Operation ()
operationPopVariableScope = Operation $ do
  cpu <- get
  put cpu {cpuVar = S.drop 1 (cpuVar cpu)}

getIntegral :: (Integral a) => Variable -> a
getIntegral (I8 x) = fromIntegral x
getIntegral (I16 x) = fromIntegral x
getIntegral (I32 x) = fromIntegral x
getIntegral (I64 x) = fromIntegral x
getIntegral (U8 x) = fromIntegral x
getIntegral (U16 x) = fromIntegral x
getIntegral (U32 x) = fromIntegral x
getIntegral (U64 x) = fromIntegral x
getIntegral _ = 0

getFloating :: (RealFloat a) => Variable -> a
getFloating (F32 x) = realToFrac x
getFloating (F64 x) = realToFrac x
getFloating _ = 0

exec :: Instruction -> Operation ()
exec (Funk, v) = do
  cpu <- Operation get
  operationPushFunk (ip cpu)
  operationJump (getIntegral v :: Int)
exec (Iload, v) = operationAddIp >> operationLoadVar (getIntegral v)
exec (Fload, v) = operationAddIp >> operationLoadVar (getIntegral v)
exec (Uload, v) = operationAddIp >> operationLoadVar (getIntegral v)
exec (Istore, v) = operationAddIp >> operationStoreVar (getIntegral v)
exec (Fstore, v) = operationAddIp >> operationStoreVar (getIntegral v)
exec (Ustore, v) = operationAddIp >> operationStoreVar (getIntegral v)
exec (Iconst, v) = operationAddIp >> operationPushStack v
exec (Fconst, v) = operationAddIp >> operationPushStack v
exec (Uconst, v) = operationAddIp >> operationPushStack v
exec (Iadd, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (addI a b)
exec (Fadd, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (addF a b)
exec (Isub, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (subI a b)
exec (Fsub, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (subF a b)
exec (Imul, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (mulI a b)
exec (Fmul, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (mulF a b)
exec (Idiv, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (divI a b)
exec (Fdiv, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (divF a b)
exec (Irem, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (modI a b)
exec (Ieq, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (eqI a b)
exec (Feq, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (eqF a b)
exec (Ine, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (neI a b)
exec (Fne, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (neF a b)
exec (Ilt, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (ltI a b)
exec (Flt, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (ltF a b)
exec (Igt, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (gtI a b)
exec (Fgt, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (gtF a b)
exec (Ile, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (leI a b)
exec (Fle, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (leF a b)
exec (Ige, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (geI a b)
exec (Fge, _) = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (geF a b)
exec (Ift, v) = do
  a <- operationPopStack
  if (getIntegral a :: Int) == 1
    then operationJump (getIntegral v :: Int)
    else operationAddIp
exec (Iff, v) = do
  a <- operationPopStack
  if (getIntegral a :: Int) == 1
    then operationAddIp
    else operationJump (getIntegral v :: Int)
exec (Goto, v) = operationJump (getIntegral v :: Int)
exec (Iand, _) = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (andI a b)
exec (Ior, _) = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (orI a b)
exec (Ixor, _) = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (xorI a b)
exec (Invoke, v) = do
  cpu <- Operation get
  operationAddIp
  operationPushStack (I64 (fromIntegral (fp cpu)))
  operationPushStack (I64 (fromIntegral (ip cpu + 1)))
  operationSetFp (S.length (cpuStack cpu) - 1)
  operationSetIp (S.index (cpuFunk cpu) (S.length (cpuFunk cpu) - 1 - getIntegral v))
  operationAddIp
  operationPushVariableScope
exec (Return, _) = do
  cpu <- Operation get
  if fp cpu == -1
    then do
      a <- operationPopStack
      operationSetIp (S.length (instructions cpu))
      operationSetExitCode (getIntegral a :: Int)
      return ()
    else do
      ipt <- operationPopStackIndex (S.length (cpuStack cpu) - 1 - fp cpu - 2)
      fpt <- operationPopStackIndex (S.length (cpuStack cpu) - 1 - fp cpu - 2)
      operationRepeatOp (S.length (cpuStack cpu) - 1 - fp cpu - 2 - 1) popStack
      operationSetFp (getIntegral fpt)
      operationSetIp (getIntegral ipt)
      operationPopVariableScope
        where
          popStack :: Operation ()
          popStack = do
            cpu <- Operation get
            _ <- operationPopStackIndex (S.length (cpuStack cpu) - 1 - fp cpu - 2)
            return ()
exec (Pop, v) = do
  operationAddIp
  operationRepeatOp (getIntegral v) operationPopStackDiscard
  return ()
exec (PopPrev, v) = do
  operationAddIp
  a <- operationPopStack
  operationRepeatOp (getIntegral v) operationPopStackDiscard
  operationPushStack a
exec (IloadStack, v) = do
  cpu <- Operation get
  operationAddIp
  a <- operationGetStackIndex (S.length (cpuStack cpu) - 1 - fp cpu - getIntegral v)
  operationPushStack a
exec (FloadStack, v) = do
  cpu <- Operation get
  operationAddIp
  a <- operationGetStackIndex (S.length (cpuStack cpu) - 1 - fp cpu - getIntegral v)
  operationPushStack a
exec (UloadStack, v) = do
  cpu <- Operation get
  operationAddIp
  a <- operationGetStackIndex (S.length (cpuStack cpu) - 1 - fp cpu - getIntegral v)
  operationPushStack a
exec (Iconvert, v) = do
  operationAddIp
  a <- operationPopStack
  case getIntegral v :: Int of
    1 -> operationPushStack (I8 (getIntegral a))
    2 -> operationPushStack (I16 (getIntegral a))
    4 -> operationPushStack (I32 (getIntegral a))
    8 -> operationPushStack (I64 (getIntegral a))
    _ -> operationPushStack (I64 (getIntegral a))
exec (Fconvert, v) = do
  operationAddIp
  a <- operationPopStack
  case getIntegral v :: Int of
    1 -> operationPushStack (F32 (getFloating a))
    2 -> operationPushStack (F32 (getFloating a))
    4 -> operationPushStack (F32 (getFloating a))
    8 -> operationPushStack (F64 (getFloating a))
    _ -> operationPushStack (F64 (getFloating a))
exec (Uconvert, v) = do
  operationAddIp
  a <- operationPopStack
  case getIntegral v :: Int of
    1 -> operationPushStack (U8 (getIntegral a))
    2 -> operationPushStack (U16 (getIntegral a))
    4 -> operationPushStack (U32 (getIntegral a))
    8 -> operationPushStack (U64 (getIntegral a))
    _ -> operationPushStack (U64 (getIntegral a))
exec (Addr, v) = do
  operationAddIp
  operationAddr v
exec (Access, _) = do
  operationAddIp
  a <- operationPopStack
  operationAccess (getIntegral a)
exec (Modify, _) = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationModify (getIntegral b) a
exec (Write, _) = do
  operationAddIp
  a <- operationPopStack
  operationAddStdOut a
exec (Allocate, _) = do
  operationAddIp
  a <- operationPopStack
  operationGetHeap (getIntegral a)
exec (GetArg, _) = do
  operationAddIp
  a <- operationPopStack
  operationGetArg (getIntegral a)
exec _ = do
  cpu <- Operation get
  operationSetIp (ip cpu + 1)

execOp :: Operation ()
execOp = do
  cpu <- Operation get
  if ip cpu >= S.length (instructions cpu)
  -- if loop cpu == 1
    then return ()
    else do
      exec (instructions cpu `S.index` ip cpu)
      operationAddLoop
      execOp

execArgs :: [String] -> Operation ()
execArgs [] = return ()
execArgs (x:xs) = Operation $ do
  cpu <- get
  put cpu {args = U64 (fromIntegral (S.length (cpuStack cpu))) <| args cpu}
  runOperation (pushArgs x)
  runOperation (execArgs xs)
  where
    pushArgs :: String -> Operation ()
    pushArgs [] = Operation $ do
      cpu <- get
      put cpu {cpuStack = U8 0 <| cpuStack cpu}
      return ()
    pushArgs (x':xs') = Operation $ do
      cpu <- get
      put cpu {cpuStack = U8 (fromIntegral (fromEnum x')) <| cpuStack cpu}
      runOperation (pushArgs xs')

mainTest :: S.Seq Instruction -> [String] -> IO ()
mainTest i args' = do
  -- print i
  -- putStrLn ""
  let cpu = execState (runOperation (execArgs args')) emptyCpu
  let cpu' = execState (runOperation (operationSetInstruction i)) cpu
  let cpu'' = execState (runOperation execOp) cpu'
  putStr (stdOut cpu'')
  exitWith (if exitCode cpu'' == 0 then ExitSuccess else ExitFailure (exitCode cpu''))
