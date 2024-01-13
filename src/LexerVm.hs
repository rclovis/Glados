-- {-# LANGUAGE InstanceSigs #-}
module LexerVm
  ( vmToken,
    OpCode (..),
    Variable (..),
    Instruction,
  )
where

-- import Ast.Ast (factorial)
import Control.Applicative (Alternative (..))
import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

data OpCode
  = Funk
  | Iload
  | Fload
  | Uload
  | Istore
  | Fstore
  | Ustore
  | Iconst
  | Fconst
  | Uconst
  | Iadd
  | Fadd
  | Isub
  | Fsub
  | Imul
  | Fmul
  | Idiv
  | Fdiv
  | Irem
  | Ieq
  | Ine
  | Ilt
  | Igt
  | Ile
  | Ige
  | Feq
  | Fne
  | Flt
  | Fgt
  | Fle
  | Fge
  | Ift
  | Iff
  | Goto
  | Iand
  | Ior
  | Ixor
  | Invoke
  | Return
  | I2f
  | F2i
  | Pop
  | Dup
  | PopPrev
  | IloadStack
  | FloadStack
  | UloadStack
  | Not
  | Iconvert
  | Fconvert
  | Uconvert
  | Addr
  | Access
  | Modify
  | Write
  | Allocate
  | GetArg
  deriving (Show, Eq, Enum)

data Variable
  = I8 Int8
  | I16 Int16
  | I32 Int32
  | I64 Int64
  | F32 Float
  | F64 Double
  | U8 Word8
  | U16 Word16
  | U32 Word32
  | U64 Word64
  | None
  deriving (Show, Eq)

type Instruction = (Int, OpCode, Variable)

getWord8Value :: Word8 -> Get ()
getWord8Value exValue = do
  value <- getWord8
  if value == exValue
    then return ()
    else fail "Invalid value"

getInstruction :: Get OpCode
getInstruction =
  Funk
    <$ getWord8Value 0
      <|> Iload
    <$ getWord8Value 1
      <|> Fload
    <$ getWord8Value 2
      <|> Uload
    <$ getWord8Value 3
      <|> Istore
    <$ getWord8Value 4
      <|> Fstore
    <$ getWord8Value 5
      <|> Ustore
    <$ getWord8Value 6
      <|> Iconst
    <$ getWord8Value 7
      <|> Fconst
    <$ getWord8Value 8
      <|> Uconst
    <$ getWord8Value 9
      <|> Iadd
    <$ getWord8Value 10
      <|> Fadd
    <$ getWord8Value 11
      <|> Isub
    <$ getWord8Value 12
      <|> Fsub
    <$ getWord8Value 13
      <|> Imul
    <$ getWord8Value 14
      <|> Fmul
    <$ getWord8Value 15
      <|> Idiv
    <$ getWord8Value 16
      <|> Fdiv
    <$ getWord8Value 17
      <|> Irem
    <$ getWord8Value 18
      <|> Ieq
    <$ getWord8Value 19
      <|> Ine
    <$ getWord8Value 20
      <|> Ilt
    <$ getWord8Value 21
      <|> Igt
    <$ getWord8Value 22
      <|> Ile
    <$ getWord8Value 23
      <|> Ige
    <$ getWord8Value 24
      <|> Feq
    <$ getWord8Value 25
      <|> Fne
    <$ getWord8Value 26
      <|> Flt
    <$ getWord8Value 27
      <|> Fgt
    <$ getWord8Value 28
      <|> Fle
    <$ getWord8Value 29
      <|> Fge
    <$ getWord8Value 30
      <|> Ift
    <$ getWord8Value 31
      <|> Iff
    <$ getWord8Value 32
      <|> Goto
    <$ getWord8Value 33
      <|> Iand
    <$ getWord8Value 34
      <|> Ior
    <$ getWord8Value 35
      <|> Ixor
    <$ getWord8Value 36
      <|> Invoke
    <$ getWord8Value 37
      <|> Return
    <$ getWord8Value 38
      <|> I2f
    <$ getWord8Value 39
      <|> F2i
    <$ getWord8Value 40
      <|> Pop
    <$ getWord8Value 41
      <|> Dup
    <$ getWord8Value 42
      <|> PopPrev
    <$ getWord8Value 43
      <|> IloadStack
    <$ getWord8Value 44
      <|> FloadStack
    <$ getWord8Value 45
      <|> UloadStack
    <$ getWord8Value 46
      <|> Not
    <$ getWord8Value 47
      <|> Iconvert
    <$ getWord8Value 48
      <|> Fconvert
    <$ getWord8Value 49
      <|> Uconvert
    <$ getWord8Value 50
      <|> Addr
    <$ getWord8Value 51
      <|> Access
    <$ getWord8Value 52
      <|> Modify
    <$ getWord8Value 53
      <|> Write
    <$ getWord8Value 54
      <|> Allocate
    <$ getWord8Value 55
      <|> GetArg
    <$ getWord8Value 56


getVariableI :: Get Variable
getVariableI = do
  value <- getWord8
  case value of
    1 -> I8 <$> getInt8
    2 -> I16 <$> getInt16be
    4 -> I32 <$> getInt32be
    8 -> I64 <$> getInt64be
    _ -> fail "Invalid value"

getVariableF :: Get Variable
getVariableF = do
  value <- getWord8
  case value of
    4 -> F32 <$> getFloatbe
    8 -> F64 <$> getDoublebe
    _ -> fail "Invalid value"

getVariableU :: Get Variable
getVariableU = do
  value <- getWord8
  case value of
    1 -> U8 <$> getWord8
    2 -> U16 <$> getWord16be
    4 -> U32 <$> getWord32be
    8 -> U64 <$> getWord64be
    _ -> fail "Invalid value"

getVariableN :: Get Variable
getVariableN = return None

lenOfVar :: Variable -> Int
lenOfVar (I8 _) = 1
lenOfVar (I16 _) = 2
lenOfVar (I32 _) = 4
lenOfVar (I64 _) = 8
lenOfVar (F32 _) = 4
lenOfVar (F64 _) = 8
lenOfVar (U8 _) = 1
lenOfVar (U16 _) = 2
lenOfVar (U32 _) = 4
lenOfVar (U64 _) = 8
lenOfVar None = -1

getAndWith :: (a -> b -> c) -> Get a -> Get b -> Get c
getAndWith p0 p1 p2 = do
  a <- p1
  p0 a <$> p2

getMany :: Get a -> Get [a]
getMany p = do
  mt <- isEmpty
  if mt
    then return []
    else do
      x <- p
      (x :) <$> getMany p

getHeader :: Get ()
getHeader = do
  a <- getWord32be
  if a /= 0x46554e4b
    then fail "Invalid header"
    else do
      _ <- getByteString 64
      return ()


getCouple :: Get (Int, OpCode, Variable)
getCouple = do
  opCode <- getInstruction
  var <- case opCode of
    Funk -> getVariableI
    Iload -> getVariableI
    Fload -> getVariableI
    Uload -> getVariableI
    Istore -> getVariableI
    Fstore -> getVariableI
    Ustore -> getVariableI
    Iconst -> getVariableI
    Fconst -> getVariableF
    Uconst -> getVariableU
    Iadd -> getVariableN
    Fadd -> getVariableN
    Isub -> getVariableN
    Fsub -> getVariableN
    Imul -> getVariableN
    Fmul -> getVariableN
    Idiv -> getVariableN
    Fdiv -> getVariableN
    Irem -> getVariableN
    Ieq -> getVariableN
    Ine -> getVariableN
    Ilt -> getVariableN
    Igt -> getVariableN
    Ile -> getVariableN
    Ige -> getVariableN
    Feq -> getVariableN
    Fne -> getVariableN
    Flt -> getVariableN
    Fgt -> getVariableN
    Fle -> getVariableN
    Fge -> getVariableN
    Ift -> getVariableI
    Iff -> getVariableI
    Goto -> getVariableI
    Iand -> getVariableN
    Ior -> getVariableN
    Ixor -> getVariableN
    Invoke -> getVariableI
    Return -> getVariableN
    I2f -> getVariableN
    F2i -> getVariableN
    Pop -> getVariableI
    Dup -> getVariableI
    PopPrev -> getVariableI
    IloadStack -> getVariableI
    FloadStack -> getVariableI
    UloadStack -> getVariableI
    Not -> getVariableN
    Iconvert -> getVariableI
    Fconvert -> getVariableI
    Uconvert -> getVariableI
    Addr -> getVariableI
    Access -> getVariableN
    Modify -> getVariableN
    Write -> getVariableN
    Allocate -> getVariableN
    GetArg -> getVariableN
  return (lenOfVar var + 2, opCode, var)

vmToken :: BL.ByteString -> Maybe [Instruction]
vmToken s = case runGetOrFail (getAndWith (\_ y -> y) getHeader (getMany getCouple)) s of
  Left _ -> Nothing
  Right (_, _, x) -> Just x
