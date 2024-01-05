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
    divF,
    divI,
    modI,
    mulF,
    mulI,
    subF,
    subI,
  )
import System.Environment (getArgs)

data Cpu = Cpu
  { ip :: Int, -- Instruction pointer
    fp :: Int, -- Frame pointer
    cpuStack :: S.Seq Variable, -- The stack
    cpuFunk :: S.Seq Int, -- The function stack
    cpuVar :: S.Seq (S.Seq Variable), -- Gloal variables
    ranOp :: Int, -- The last opcode that was executed
    cpuState :: Int -- The status of the program
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
      cpuState = 0
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

operationPopStackIndex :: Int -> Operation Variable
operationPopStackIndex x = Operation $ do
  cpu <- get
  let i = cpuStack cpu `S.index` x
  put cpu {cpuStack = S.deleteAt x (cpuStack cpu)}
  return i

operationPushStack :: Variable -> Operation ()
operationPushStack x = Operation $ do
  cpu <- get
  put cpu {cpuStack = x <| cpuStack cpu}

operationExtendVar :: Int -> Operation ()
operationExtendVar x = Operation $ do
  cpu <- get
  put cpu {cpuVar = S.replicate x S.empty >< cpuVar cpu}

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

operationPushVar :: Variable -> Operation ()
operationPushVar x = Operation $ do
  cpu <- get
  put cpu {cpuVar = S.singleton x <| cpuVar cpu}

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

getIntegral :: (Integral a) => Variable -> a
getIntegral (I8 x) = fromIntegral x
getIntegral (I16 x) = fromIntegral x
getIntegral (I32 x) = fromIntegral x
getIntegral (I64 x) = fromIntegral x
getIntegral (U8 x) = fromIntegral x
getIntegral (U16 x) = fromIntegral x
getIntegral (U32 x) = fromIntegral x
getIntegral (U64 x) = fromIntegral x
getIntegral (ISize x) = fromIntegral x
getIntegral (USize x) = fromIntegral x
getIntegral (Bool True) = 1
getIntegral (Bool False) = 0
getIntegral _ = 0

getFloating :: (RealFloat a) => Variable -> a
getFloating (F32 x) = realToFrac x
getFloating (F64 x) = realToFrac x
getFloating _ = 0

getBool :: Variable -> Bool
getBool (Bool x) = x
getBool _ = False

exec :: Instruction -> [Instruction] -> Operation ()
exec (_, Funk, v) i = do
  cpu <- Operation get
  operationPushFunk (ip cpu)
  operationSetIp (ip cpu + 1)
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
      execOp i

mainTest :: [Instruction] -> IO ()
mainTest i = do
  print i
  let cpu' = execState (runOperation (execOp i)) emptyCpu
  print cpu'