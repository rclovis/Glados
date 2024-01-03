module Bytecode (IntTypes (..), getBin, bytecode, getIEEE, floatingStandardtoWord8, word8toChar, WordTypes (..), wordTypesTo8bit) where

import Data.Char (digitToInt, intToDigit, chr)

import Data.Int (Int8, Int16, Int32, Int64)

import Data.Bits (shiftR, (.&.))

import Data.Word (Word8, Word16, Word32, Word64)

import qualified Data.ByteString as B

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

word8toChar :: [Word8] -> B.ByteString
word8toChar = B.pack

intTypesTo8bit :: IntTypes -> [Word8]
intTypesTo8bit intType =
    case intType of
        Int8Val val -> reverse $ splitInt8 val
        Int16Val val -> reverse $ splitInt16 val
        Int32Val val -> reverse $ splitInt32 val
        Int64Val val -> reverse $ splitInt64 val
  where
    splitInt8 :: Int8 -> [Word8]
    splitInt8 val = [fromIntegral val]

    splitInt16 :: Int16 -> [Word8]
    splitInt16 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0..1]]

    splitInt32 :: Int32 -> [Word8]
    splitInt32 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0..3]]

    splitInt64 :: Int64 -> [Word8]
    splitInt64 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0..7]]

wordTypesTo8bit :: WordTypes -> [Word8]
wordTypesTo8bit wordType =
    case wordType of
        Word8Val val -> reverse $ splitWord8 val
        Word16Val val -> reverse $ splitWord16 val
        Word32Val val -> reverse $ splitWord32 val
        Word64Val val -> reverse $ splitWord64 val
  where
    splitWord8 :: Word8 -> [Word8]
    splitWord8 val = [val]

    splitWord16 :: Word16 -> [Word8]
    splitWord16 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0..1]]

    splitWord32 :: Word32 -> [Word8]
    splitWord32 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0..3]]

    splitWord64 :: Word64 -> [Word8]
    splitWord64 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0..7]]

-- transfrom binary string to list of Word8
floatingStandardtoWord8 :: [Char] -> [Word8]
floatingStandardtoWord8 [] = []
floatingStandardtoWord8 str = toWord8 (take 8 str) : floatingStandardtoWord8 (drop 8 str)
  where
    toWord8 :: [Char] -> Word8
    toWord8 st = toEnum $ foldl (\acc x -> acc * 2 + digitToInt x) 0 st

floatingPointToW8 :: FloatingPoint -> [Word8]
floatingPointToW8 (FloatVal f) = floatingStandardtoWord8 $ getIEEE f
floatingPointToW8 (DoubleVal d) = floatingStandardtoWord8 $ getDIEEE d

getDIEEE :: Double -> [Char]
getDIEEE f = getSignD f ++ getExponentD t ++ getMantissaD m
  where
    t = toBinary $ abs (truncate f)
    m = tail (doubleToBinary $ abs f)

doubleToBinary :: Double -> [Char]
doubleToBinary f =
  let integerPart = abs (truncate f)
      fractionalPart = abs f - fromIntegral integerPart
      intBinary = toBinary integerPart
      fracBinary = dfractionToBinary fractionalPart
  in intBinary ++ fracBinary

dfractionToBinary :: Double -> [Char]
dfractionToBinary f = dfracToBinStr f []
  where
    dfracToBinStr _ acc | length acc >= 52 = acc -- Max 52-bit precision for demonstration
    dfracToBinStr 0 acc = acc
    dfracToBinStr x acc =
      let dbl = x * 2
          (i, f') = properFraction dbl :: (Int, Double)
      in dfracToBinStr f' (acc ++ show i)

getMantissaD :: [Char] -> [Char]
getMantissaD f = f ++ padZeros (52 - length f) (toBinary 0)

getExponentD :: [Char] -> [Char]
getExponentD f = padZeros 11 (toBinary (length f - 1 + 1023))

getSignD :: Double -> [Char]
getSignD f
  | f < 0 = "1"
  | otherwise = "0"

getIEEE :: Float -> [Char]
getIEEE f = getSign f ++ getExponent t ++ getMantissa m
  where
    t = toBinary $ abs (truncate f)
    m = tail (floatToBinary $ abs f)

toBinary :: Int -> [Char]
toBinary 0 = ""
toBinary n = reverse $ toBinStr n
  where
    toBinStr 0 = []
    toBinStr x = let (q, r) = x `divMod` 2 in head (show r) : toBinStr q

-- Function to convert a fraction to binary string
fractionToBinary :: Float -> [Char]
fractionToBinary f = fracToBinStr f []
  where
    fracToBinStr _ acc | length acc >= 32 = acc -- Max 32-bit precision for demonstration
    fracToBinStr 0 acc = acc
    fracToBinStr x acc =
      let dbl = x * 2
          (i, f') = properFraction dbl :: (Int, Float)
      in fracToBinStr f' (acc ++ show i)

-- Function to represent a float in binary
floatToBinary :: Float -> [Char]
floatToBinary f =
  let integerPart = abs (truncate f)
      fractionalPart = abs f - fromIntegral integerPart
      intBinary = toBinary integerPart
      fracBinary = fractionToBinary fractionalPart
  in intBinary ++ fracBinary

getMantissa :: [Char] -> [Char]
getMantissa f = f ++ padZeros (23 - length f) (toBinary 0)

getExponent :: [Char] -> [Char]
getExponent f = padZeros 8 (toBinary (length f - 1 + 127))

getSign :: Float -> [Char]
getSign f
  | f < 0 = "1"
  | otherwise = "0"

toHexa :: Int -> Char
toHexa n
  | n < 10 = toEnum (n + fromEnum '0')
  | otherwise = toEnum (n - 10 + fromEnum 'A')

padZeros :: Int -> [Char] -> [Char]
padZeros len str = replicate (max 0 (len - length str)) '0' ++ str

getBin :: [Bytecode] -> B.ByteString
getBin = B.pack . concatMap toBin

toBin :: Bytecode -> [Word8]
toBin (Funk a b) = [0, a, b]
toBin (Iload a b) = [1, a, b]
toBin (Fload a b) = [2, a, b]
toBin (Uload a b) = [3, a, b]
toBin (Istore a b) = [4, a, b]
toBin (Fstore a b) = [5, a, b]
toBin (Ustore a b) = [6, a, b]
toBin (Iconst a b) = [7, a] ++ intTypesTo8bit b
toBin (Fconst a b) = [8, a] ++ floatingPointToW8 b
toBin (Uconst a b) = [9, a] ++ wordTypesTo8bit b
toBin Iadd = [10]
toBin Fadd = [11]
toBin Isub = [12]
toBin Fsub = [13]
toBin Imul = [14]
toBin Fmul = [15]
toBin Idiv = [16]
toBin Fdiv = [17]
toBin Imod = [18]
toBin Ieq = [19]
toBin Feq = [20]
toBin Ineq = [21]
toBin Fneq = [22]
toBin Igt = [23]
toBin Fgt = [24]
toBin Ilt = [25]
toBin Flt = [26]
toBin Ige = [27]
toBin Fge = [28]
toBin Ile = [29]
toBin Fle = [30]
toBin Iand = [31]
toBin Ior = [32]
toBin Ixor = [33]
toBin (Ift a b) = [34, a] ++ intTypesTo8bit (Int32Val (fromIntegral b))
toBin (Iff a b) = [35, a] ++ intTypesTo8bit (Int32Val (fromIntegral b))
toBin (Goto a b) = [36, a] ++ intTypesTo8bit (Int32Val (fromIntegral b))
toBin (Invoke a b) = [37, a] ++ intTypesTo8bit (Int32Val (fromIntegral b))
toBin Return = [38]
toBin I2f = [39]
toBin F2i = [40]
toBin (Consume a b) = [41, a, b]
toBin NOTHING = []

getHumanReadable :: [Bytecode] -> [Char]
getHumanReadable = concatMap toHumanReadable

toHumanReadable :: Bytecode -> [Char]
toHumanReadable (Funk _ b) = "Funk " ++ show b ++ "\n"
toHumanReadable (Iload _ b) = " Iload " ++ show b ++ "\n"
toHumanReadable (Fload _ b) = " Fload " ++ show b ++ "\n"
toHumanReadable (Uload _ b) = " Uload " ++ show b ++ "\n"
toHumanReadable (Istore _ b) = "  Istore " ++ show b ++ "\n"
toHumanReadable (Fstore _ b) = "  Fstore " ++ show b ++ "\n"
toHumanReadable (Ustore _ b) = "  Ustore " ++ show b ++ "\n"
toHumanReadable (Iconst _ b) = "  Iconst " ++ show b ++ "\n"
toHumanReadable (Fconst _ b) = "  Fconst " ++ show b ++ "\n"
toHumanReadable (Uconst _ b) = "  Uconst " ++ show b ++ "\n"
toHumanReadable Iadd = "  Iadd\n"
toHumanReadable Fadd = "  Fadd\n"
toHumanReadable Isub = "  Isub\n"
toHumanReadable Fsub = "  Fsub\n"
toHumanReadable Imul = "  Imul\n"
toHumanReadable Fmul = "  Fmul\n"
toHumanReadable Idiv = "  Idiv\n"
toHumanReadable Fdiv = "  Fdiv\n"
toHumanReadable Imod = "  Imod\n"
toHumanReadable Ieq = "  Ieq\n"
toHumanReadable Feq = "  Feq\n"
toHumanReadable Ineq = "  Ineq\n"
toHumanReadable Fneq = "  Fneq\n"
toHumanReadable Igt = "  Igt\n"
toHumanReadable Fgt = "  Fgt\n"
toHumanReadable Ilt = "  Ilt\n"
toHumanReadable Flt = "  Flt\n"
toHumanReadable Ige = "  Ige\n"
toHumanReadable Fge = "  Fge\n"
toHumanReadable Ile = "  Ile\n"
toHumanReadable Fle = "  Fle\n"
toHumanReadable Iand = "  Iand\n"
toHumanReadable Ior = "  Ior\n"
toHumanReadable Ixor = "  Ixor\n"
toHumanReadable (Ift _ b) = "  Ift " ++ show b ++ "\n"
toHumanReadable (Iff _ b) = "  Iff " ++ show b ++ "\n"
toHumanReadable (Goto _ b) = "  Goto " ++ show b ++ "\n"
toHumanReadable (Invoke _ b) = "  Invoke " ++ show b ++ "\n"
toHumanReadable Return = "  Return\n"
toHumanReadable I2f = "  I2f\n"
toHumanReadable F2i = "  F2i\n"
toHumanReadable (Consume _ b) = "  Consume " ++ show b ++ "\n"
toHumanReadable NOTHING = "\n"

bytecode :: [Bytecode]
bytecode =
    [
      Funk 1 26,
      Iload 4 1,
      Iload 4 2,
      Iadd,
      Istore 4 3,
      Iload 4 3,
      Return,
      Iconst 4 (Int32Val 1),
      Iconst 4 (Int32Val 2),
      Invoke 1 0,
      Uconst 2 (Word16Val 255),
      Fconst 8 (DoubleVal 154541.452)
    ]
