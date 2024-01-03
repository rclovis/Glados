module LexerVm
  ( vmToken,
    OpCode (..),
    Variable (..),
    Instruction,
  )
where

import Ast (factorial)
import Control.Applicative (Alternative (..))
import Data.Int
import Data.Char
import Data.List

import Data.Word
import Eval (exec)
import Parser
  ( Parser (..),
    parseAnd,
    parseAndWith,
    parseAnyChar,
    parseChar,
    parseFloat,
    parseInt,
    parseList,
    parseMany,
    parseNothing,
    parseOr,
    parseQuantity,
    parseSome,
    parseString,
    parseUFloat,
    parseUInt,
    runParser,
    parseOneChar,
    parseByte,
  )

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
  deriving (Show, Eq, Enum)

data Variable
  = I8 Int8
  | I16 Int16
  | I32 Int32
  | I64 Int64
  | F32 Float
  | F64 Double
  | ISize Int
  | U8 Word8
  | U16 Word16
  | U32 Word32
  | U64 Word64
  | USize Word
  | Bool Bool
  | None
  deriving (Show, Eq)

type Instruction = (Int, OpCode, Variable)

parserInstruction :: Parser OpCode
parserInstruction =
    Funk <$ parseByte 0 <|>
    Iload <$ parseByte 1 <|>
    Fload <$ parseByte 2 <|>
    Uload <$ parseByte 3 <|>
    Istore <$ parseByte 4 <|>
    Fstore <$ parseByte 5 <|>
    Ustore <$ parseByte 6 <|>
    Iconst <$ parseByte 7 <|>
    Fconst <$ parseByte 8 <|>
    Uconst <$ parseByte 9 <|>
    Iadd <$ parseByte 10 <|>
    Fadd <$ parseByte 11 <|>
    Isub <$ parseByte 12 <|>
    Fsub <$ parseByte 13 <|>
    Imul <$ parseByte 14 <|>
    Fmul <$ parseByte 15 <|>
    Idiv <$ parseByte 16 <|>
    Fdiv <$ parseByte 17 <|>
    Irem <$ parseByte 18 <|>
    Ieq <$ parseByte 19 <|>
    Ine <$ parseByte 20 <|>
    Ilt <$ parseByte 21 <|>
    Igt <$ parseByte 22 <|>
    Ile <$ parseByte 23 <|>
    Ige <$ parseByte 24 <|>
    Feq <$ parseByte 25 <|>
    Fne <$ parseByte 26 <|>
    Flt <$ parseByte 27 <|>
    Fgt <$ parseByte 28 <|>
    Fle <$ parseByte 29 <|>
    Fge <$ parseByte 30 <|>
    Ift <$ parseByte 31 <|>
    Iff <$ parseByte 32 <|>
    Goto <$ parseByte 33 <|>
    Iand <$ parseByte 34 <|>
    Ior <$ parseByte 35 <|>
    Ixor <$ parseByte 36 <|>
    Invoke <$ parseByte 37 <|>
    Return <$ parseByte 38 <|>
    I2f <$ parseByte 39 <|>
    F2i <$ parseByte 40

getIntegerFromBytes :: Integral a => [Char] -> a
getIntegerFromBytes = foldl' (\acc x -> acc * 256 + fromIntegral (ord x)) 0

getI8 :: String -> Int8
getI8 = getIntegerFromBytes

getI16 :: String -> Int16
getI16 = getIntegerFromBytes

getI32 :: String -> Int32
getI32 = getIntegerFromBytes

getI64 :: String -> Int64
getI64 = getIntegerFromBytes

parserVariableI :: Parser Variable
parserVariableI = Parser f
  where
    f :: String -> Maybe (Variable, String)
    f (x : xs) = case ord x of
      1 -> Just (I8 (getI8 (take 1 xs)), drop 1 xs)
      2 -> Just (I16 (getI16 (take 2 xs)), drop 2 xs)
      4 -> Just (I32 (getI32 (take 4 xs)), drop 4 xs)
      8 -> Just (I64 (getI64 (take 8 xs)), drop 8 xs)
      _ -> Nothing
    f [] = Nothing

parserVariableN :: Parser Variable
parserVariableN = None <$ parseNothing

lenOfVar :: Variable -> Int
lenOfVar (I8 _) = 1
lenOfVar (I16 _) = 2
lenOfVar (I32 _) = 4
lenOfVar (I64 _) = 8
lenOfVar (F32 _) = 4
lenOfVar (F64 _) = 8
lenOfVar (ISize _) = 8
lenOfVar (U8 _) = 1
lenOfVar (U16 _) = 2
lenOfVar (U32 _) = 4
lenOfVar (U64 _) = 8
lenOfVar (USize _) = 8
lenOfVar (Bool _) = 1
lenOfVar None = 0

parserCouple :: Parser (Int, OpCode, Variable)
parserCouple = Parser f
  where
    f :: String -> Maybe ((Int, OpCode, Variable), String)
    f s = case runParser parserInstruction s of
      Just (Funk, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Iload, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Fload, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Uload, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Istore, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Fstore, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Ustore, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Iconst, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Fconst, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Uconst, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Iadd, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Fadd, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Isub, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Fsub, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Imul, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Fmul, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Idiv, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Fdiv, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Irem, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Ieq, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Ine, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Ilt, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Igt, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Ile, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Ige, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Feq, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Fne, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Flt, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Fgt, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Fle, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Fge, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Ift, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Iff, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Goto, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Iand, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Ior, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Ixor, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (Invoke, _) -> runParser (parseAndWith (\x y -> (lenOfVar y + 2, x, y)) parserInstruction parserVariableI) s
      Just (Return, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (I2f, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Just (F2i, _) -> runParser (parseAndWith (\x y -> (1, x, y)) parserInstruction parserVariableN) s
      Nothing -> Nothing

vmToken :: String -> Maybe [Instruction]
vmToken s = case runParser (parseMany parserCouple) s of
  Just (t, "") -> Just t
  _ -> Nothing