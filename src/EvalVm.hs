{-# LANGUAGE InstanceSigs #-}

module EvalVm
  ( mainTest,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.State
import Data.Int
import Data.Sequence as S
import Data.Word
import LexerVm
  ( Instruction,
    OpCode (..),
    Variable (..),
    vmToken,
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
import System.Environment (getArgs)

data Cpu = Cpu
  { ip :: Int, -- Instruction pointer
    fp :: Int, -- Frame pointer
    cpuStack :: S.Seq Variable, -- The stack
    cpuFunk :: S.Seq Int, -- The function stack
    cpuVar :: S.Seq (S.Seq Variable), -- Gloal variables
    ranOp :: Int, -- The last opcode that was executed
    cpuState :: Int, -- The status of the program
    loop :: Int -- The loop counter
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
      loop = 0
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
    cpu <- get
    return undefined

  (<|>) :: Operation a -> Operation a -> Operation a
  (Operation op1) <|> (Operation op2) = Operation $ do
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

operationSetIp :: Int -> Operation ()
operationSetIp x = Operation $ do
  cpu <- get
  put cpu {ip = x}

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
  let (x :<| xs) = cpuStack cpu
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

operationSetVar :: Int -> Variable -> Operation ()
operationSetVar x y = Operation $ do
  cpu <- get
  case cpuVar cpu of
    (cpuVarTop :<| xs) -> do
      put cpu {cpuVar = S.update x y cpuVarTop <| xs}
    _ -> return ()

operationGetVar :: Int -> Operation Variable
operationGetVar x = Operation $ do
  cpu <- get
  case cpuVar cpu of
    (cpuVarTop :<| _) -> do
      return $ cpuVarTop `S.index` x
    _ -> return None

operationLoadVar :: Int -> Operation ()
operationLoadVar x = Operation $ do
  cpu <- get
  case cpuVar cpu of
    (cpuVarTop :<| _) -> do
      let i = cpuVarTop `S.index` x
      runOperation (operationPushStack i)
    _ -> return ()

operationStoreVar :: Int -> Operation ()
operationStoreVar x = Operation $ do
  pop <- runOperation operationPopStack
  runOperation $ operationSetVar x pop

operationRepeatOp :: Int -> Operation () -> Operation ()
operationRepeatOp 0 _ = return ()
operationRepeatOp x op = do
  op
  operationRepeatOp (x - 1) op

operationJump :: (Integral a) => a -> [Instruction] -> Operation ()
operationJump 0 _ = return ()
operationJump x i = Operation $ do
  cpu <- get
  if x < 0
    then do
      let (l, _, _) = i !! ip cpu
      put cpu {ip = ip cpu - 1}
      runOperation $ operationJump (fromIntegral x - l) i
    else do
      let (l, _, _) = i !! ip cpu
      put cpu {ip = ip cpu + 1}
      runOperation $ operationJump (fromIntegral x - l) i
  return ()

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

exec :: Instruction -> [Instruction] -> Operation ()
exec (_, Funk, v) i = do
  cpu <- Operation get
  operationPushFunk (ip cpu)
  operationJump (getIntegral v) i
exec (_, Iload, v) _ = operationAddIp >> operationLoadVar (getIntegral v)
exec (_, Fload, v) _ = operationAddIp >> operationLoadVar (getIntegral v)
exec (_, Uload, v) _ = operationAddIp >> operationLoadVar (getIntegral v)
exec (_, Istore, v) _ = operationAddIp >> operationStoreVar (getIntegral v)
exec (_, Fstore, v) _ = operationAddIp >> operationStoreVar (getIntegral v)
exec (_, Ustore, v) _ = operationAddIp >> operationStoreVar (getIntegral v)
exec (_, Iconst, v) _ = operationAddIp >> operationPushStack v
exec (_, Fconst, v) _ = operationAddIp >> operationPushStack v
exec (_, Uconst, v) _ = operationAddIp >> operationPushStack v
exec (_, Iadd, _) _ = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (addI a b)
exec (_, Fadd, _) _ = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (addF a b)
exec (_, Isub, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (subI a b)
exec (_, Fsub, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (subF a b)
exec (_, Imul, _) _ = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (mulI a b)
exec (_, Fmul, _) _ = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (mulF a b)
exec (_, Idiv, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (divI a b)
exec (_, Fdiv, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (divF a b)
exec (_, Irem, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (modI a b)
exec (_, Ieq, _) _ = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (eqI a b)
exec (_, Feq, _) _ = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (eqF a b)
exec (_, Ine, _) _ = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (neI a b)
exec (_, Fne, _) _ = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (neF a b)
exec (_, Ilt, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (ltI a b)
exec (_, Flt, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (ltF a b)
exec (_, Igt, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (gtI a b)
exec (_, Fgt, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (gtF a b)
exec (_, Ile, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (leI a b)
exec (_, Fle, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (leF a b)
exec (_, Ige, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (geI a b)
exec (_, Fge, _) _ = do
  operationAddIp
  b <- operationPopStack
  a <- operationPopStack
  operationPushStack (geF a b)
exec (_, Ift, v) i = do
  a <- operationPopStack
  if getIntegral a == 1
    then operationJump (getIntegral v) i
    else operationAddIp
exec (_, Iff, v) i = do
  a <- operationPopStack
  if getIntegral a == 1
    then operationAddIp
    else operationJump (getIntegral v) i
exec (_, Goto, v) i = operationJump (getIntegral v) i
exec (_, Iand, _) _ = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (andI a b)
exec (_, Ior, _) _ = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (orI a b)
exec (_, Ixor, _) _ = do
  operationAddIp
  a <- operationPopStack
  b <- operationPopStack
  operationPushStack (xorI a b)
exec (_, Invoke, v) _ = do
  cpu <- Operation get
  operationAddIp
  operationPushStack (I64 (fromIntegral (fp cpu)))
  operationPushStack (I64 (fromIntegral (ip cpu + 1)))
  operationSetFp (S.length (cpuStack cpu) - 1)
  operationSetIp (cpuFunk cpu `S.index` getIntegral v)
  operationAddIp
  operationPushVariableScope
exec (_, Return, _) _ = do
  cpu <- Operation get
  ipt <- operationPopStackIndex (S.length (cpuStack cpu) - 1 - fp cpu - 2)
  fpt <- operationPopStackIndex (S.length (cpuStack cpu) - 1 - fp cpu - 2)
  operationSetFp (getIntegral fpt)
  operationSetIp (getIntegral ipt)
  operationPopVariableScope
exec (_, Pop, v) _ = do
  operationAddIp
  operationRepeatOp (getIntegral v) operationPopStackDiscard
  return ()
exec (_, PopPrev, v) _ = do
  operationAddIp
  a <- operationPopStack
  operationRepeatOp (getIntegral v) operationPopStackDiscard
  operationPushStack a
exec (_, IloadStack, v) _ = do
  cpu <- Operation get
  operationAddIp
  a <- operationGetStackIndex (S.length (cpuStack cpu) - 1 - fp cpu - getIntegral v)
  operationPushStack a
exec (_, FloadStack, v) _ = do
  cpu <- Operation get
  operationAddIp
  a <- operationGetStackIndex (S.length (cpuStack cpu) - 1 - fp cpu - getIntegral v)
  operationPushStack a
exec (_, UloadStack, v) _ = do
  cpu <- Operation get
  operationAddIp
  a <- operationGetStackIndex (S.length (cpuStack cpu) - 1 - fp cpu - getIntegral v)
  operationPushStack a

exec _ _ = do
  cpu <- Operation get
  operationSetIp (ip cpu + 1)

execOp :: [Instruction] -> Operation ()
execOp i = do
  cpu <- Operation get
  if ip cpu >= Prelude.length i
    then return ()
    else do
      exec (i !! ip cpu) i
      operationAddLoop
      execOp i

mainTest :: [Instruction] -> IO ()
mainTest i = do
  print i
  let cpu' = execState (runOperation (execOp i)) emptyCpu
  print cpu'