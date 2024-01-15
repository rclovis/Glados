module Bytecode (getBin, bytecode, Bytecode (..), IntTypes (..), FloatingPoint (..), WordTypes (..), getHumanReadable, getSizeBytecode) where

import Data.Bits (shiftR, (.&.)) 
import qualified Data.ByteString as B
import Data.Char (digitToInt)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

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
  = Funk Word8 IntTypes -- function with int size of arg and int size of body
  | Iload Word8 IntTypes -- load int from stack
  | Fload Word8 IntTypes -- load float from stack
  | Uload Word8 IntTypes -- load uint from stack
  | Istore Word8 IntTypes -- store int to stack
  | Fstore Word8 IntTypes -- store float to stacktraverse toBin xs
  | Ustore Word8 IntTypes -- store uint to stack
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
  | Ine -- not equal int or uint
  | Ilt -- less than int or uint
  | Igt -- greater than int or uint
  | Ile -- less than or equal int or uint
  | Ige -- greater than or equal int or uint
  | Feq -- equal float
  | Fne -- not equal float
  | Flt -- less than float
  | Fgt -- greater than float
  | Fle -- less than or equal float
  | Fge -- greater than or equal float
  | Ift Word8 IntTypes -- if true jump of int bytes
  | Iff Word8 IntTypes -- if false jump of int bytes
  | Goto Word8 IntTypes -- jump of int bytes
  | Iand -- and int or uint
  | Ior -- or int or uint
  | Ixor -- xor int or uint
  | Invoke Word8 IntTypes -- invoke function at int index
  | Return -- return from function
  | I2f -- convert int to float
  | F2i -- convert float to int
  | Pop Word8 IntTypes
  | Dup Word8 IntTypes
  | PopPrev Word8 IntTypes
  | IloadStack Word8 IntTypes
  | FloadStack Word8 IntTypes
  | UloadStack Word8 IntTypes
  | Not
  | Iconvert Word8 Word8
  | Fconvert Word8 Word8
  | Uconvert Word8 Word8
  | Addr Word8 WordTypes
  | Access
  | Modify
  | Write
  | Allocate
  | GetArg
  deriving (Show, Eq)

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
    splitInt16 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0 .. 1]]

    splitInt32 :: Int32 -> [Word8]
    splitInt32 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0 .. 3]]

    splitInt64 :: Int64 -> [Word8]
    splitInt64 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0 .. 7]]

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
    splitWord16 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0 .. 1]]

    splitWord32 :: Word32 -> [Word8]
    splitWord32 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0 .. 3]]

    splitWord64 :: Word64 -> [Word8]
    splitWord64 val = [fromIntegral (val `shiftR` (8 * i)) .&. 0xFF | i <- [0 .. 7]]

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

padZeros :: Int -> [Char] -> [Char]
padZeros len str = replicate (max 0 (len - length str)) '0' ++ str

-- Funk + 32 0x00
header :: B.ByteString
header = B.pack [0x46, 0x55, 0x4E, 0x4B] `B.append` foldr B.cons B.empty (replicate 64 0x00)

getBin :: [Bytecode] -> B.ByteString
getBin xs = header `B.append` foldr B.cons B.empty (concatMap toBin xs)

toBin :: Bytecode -> [Word8]
toBin (Funk a b) = [0, a] ++ intTypesTo8bit b
toBin (Iload a b) = [1, a] ++ intTypesTo8bit b
toBin (Fload a b) = [2, a] ++ intTypesTo8bit b
toBin (Uload a b) = [3, a] ++ intTypesTo8bit b
toBin (Istore a b) = [4, a] ++ intTypesTo8bit b
toBin (Fstore a b) = [5, a] ++ intTypesTo8bit b
toBin (Ustore a b) = [6, a] ++ intTypesTo8bit b
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
toBin Ine = [20]
toBin Ilt = [21]
toBin Igt = [22]
toBin Ile = [23]
toBin Ige = [24]
toBin Feq = [25]
toBin Fne = [26]
toBin Flt = [27]
toBin Fgt = [28]
toBin Fle = [29]
toBin Fge = [30]
toBin (Ift a b) = [31, a] ++ intTypesTo8bit b
toBin (Iff a b) = [32, a] ++ intTypesTo8bit b
toBin (Goto a b) = [33, a] ++ intTypesTo8bit b
toBin Iand = [34]
toBin Ior = [35]
toBin Ixor = [36]
toBin (Invoke a b) = [37, a] ++ intTypesTo8bit b
toBin Return = [38]
toBin I2f = [39]
toBin F2i = [40]
toBin (Pop a b) = [41, a] ++ intTypesTo8bit b
toBin (Dup a b) = [42, a] ++ intTypesTo8bit b
toBin (PopPrev a b) = [43, a] ++ intTypesTo8bit b
toBin (IloadStack a b) = [44, a] ++ intTypesTo8bit b
toBin (FloadStack a b) = [45, a] ++ intTypesTo8bit b
toBin (UloadStack a b) = [46, a] ++ intTypesTo8bit b
toBin Not = [47]
toBin (Iconvert a b) = [48, a, b]
toBin (Fconvert a b) = [49, a, b]
toBin (Uconvert a b) = [50, a, b]
toBin (Addr a b) = [51, a] ++ wordTypesTo8bit b
toBin Access = [52]
toBin Modify = [53]
toBin Write = [54]
toBin Allocate = [55]
toBin GetArg = [56]

getHumanReadable :: [Bytecode] -> [Char]
getHumanReadable = concatMap toHumanReadable

toHumanReadable :: Bytecode -> [Char]
toHumanReadable (Funk _ b) = "Funk " ++ show b ++ "\n"
toHumanReadable (Iload _ b) = "  Iload " ++ show b ++ "\n"
toHumanReadable (Fload _ b) = "  Fload " ++ show b ++ "\n"
toHumanReadable (Uload _ b) = "  Uload " ++ show b ++ "\n"
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
toHumanReadable Ine = "  Ineq\n"
toHumanReadable Fne = "  Fneq\n"
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
toHumanReadable (Pop _ b) = "  Pop " ++ show b ++ "\n"
toHumanReadable (Dup _ b) = "  Dup " ++ show b ++ "\n"
toHumanReadable (PopPrev _ b) = "  PopPrev " ++ show b ++ "\n"
toHumanReadable (IloadStack _ b) = "  IloadStack " ++ show b ++ "\n"
toHumanReadable (FloadStack _ b) = "  FloadStack " ++ show b ++ "\n"
toHumanReadable (UloadStack _ b) = "  UloadStack " ++ show b ++ "\n"
toHumanReadable Not = "  Not\n"
toHumanReadable (Iconvert _ b) = "  Iconvert " ++ show b ++ "\n"
toHumanReadable (Fconvert _ b) = "  Fconvert " ++ show b ++ "\n"
toHumanReadable (Uconvert _ b) = "  Uconvert " ++ show b ++ "\n"
toHumanReadable (Addr _ b) = "  Addr " ++ show b ++ "\n"
toHumanReadable Access = "  Access\n"
toHumanReadable Modify = "  Modify\n"
toHumanReadable Write = "  Write\n"
toHumanReadable Allocate = "  Allocate\n"
toHumanReadable GetArg = "  GetArg\n"

bytecode :: [Bytecode]
bytecode =
  [ Funk 1 (Int64Val 56),
    IloadStack 1 (Int64Val 0),
    Iconst 8 (Int64Val 0),
    Ieq,
    Iff 1 (Int8Val 14),
    Iconst 8 (Int64Val 1),
    Return,
    IloadStack 1 (Int64Val 0),
    IloadStack 1 (Int64Val 0),
    Iconst 8 (Int64Val 1),
    Isub,
    Invoke 1 (Int8Val 0),
    PopPrev 1 (Int64Val 1),
    Imul,
    Return,
    Iconst 8 (Int64Val 5),
    Invoke 1 (Int8Val 0),
    PopPrev 1 (Int64Val 1)
  ]

getSizeBytecode :: Bytecode -> Int
getSizeBytecode _ = 1