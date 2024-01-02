module Bytecode ( toHexa, bytecode, getBinHexa, floatToHex, getIEEE, getHumanReadable, getFloatFromHex) where

import Data.Char (digitToInt, intToDigit)

data Bytecode
    = Funk Int Int -- function with int size of arg and int size of body
    | Iload Int Int -- load int from stack
    | Fload Int Int -- load float from stack
    | Uload Int Int -- load uint from stack
    | Istore Int Int -- store int to stack
    | Fstore Int Int -- store float to stacktraverse toBin xs
    | Ustore Int Int -- store uint to stack
    | Iconst Int Int -- push int to stack
    | Fconst Int Float -- push float to stack
    | Uconst Int Int -- push uint to stack
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
    | Ift Int Int -- if true jump of int bytes
    | Iff Int Int -- if false jump of int bytes
    | Goto Int Int -- jump of int bytes
    | Invoke Int Int -- invoke function at int index
    | Return -- return from function
    | I2f -- convert int to float
    | F2i -- convert float to int
    | NOTHING -- do nothing

toHex :: [Char] -> [Char]
toHex [] = []
toHex (a : b : c : d : xs) = intToDigit val : toHex xs
  where
    val = foldl (\acc x -> acc * 2 + digitToInt x) 0 [a, b, c, d]
toHex _ = error "Invalid binary string length"

floatToHex :: Float -> [Char]
floatToHex f = toHex $ getIEEE f 

toBinary :: Int -> [Char]
toBinary 0 = "0"
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


getIEEE :: Float -> [Char]
getIEEE f = getSign f ++ getExponent t ++ getMantissa m
  where
    t = toBinary $ abs (truncate f)
    m = tail (floatToBinary $ abs f)

getMantissa :: [Char] -> [Char]
getMantissa f = f ++ padZeros (23 - length f) (toBinary 0)

getExponent :: [Char] -> [Char]
getExponent f = padZeros 8 (toBinary (length f - 1 + 127))

getSign :: Float -> [Char]
getSign f
  | f < 0 = "1"
  | otherwise = "0"

outputWell :: Int -> Int -> [Char]
outputWell a b = padZeros (a * 2) (toHexaList b)

toHexaList :: Int -> [Char]
toHexaList 0 = "0"
toHexaList n = reverse (toHexaList' n)

toHexaList' :: Int -> [Char]
toHexaList' 0 = []
toHexaList' n = toHexa (n `mod` 16) : toHexaList' (n `div` 16)

toHexa :: Int -> Char
toHexa n
  | n < 10 = toEnum (n + fromEnum '0')
  | otherwise = toEnum (n - 10 + fromEnum 'A')

padZeros :: Int -> [Char] -> [Char]
padZeros len str = replicate (max 0 (len - length str)) '0' ++ str

getBin :: [Bytecode] -> [Char]
getBin = concatMap toBin

toBin :: Bytecode -> [Char]
toBin (Funk a b) = chr 0 ++ chr a ++ chr b


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
toHumanReadable Ieq = " Ieq\n"
toHumanReadable Feq = " Feq\n"
toHumanReadable Ineq = "  Ineq\n"
toHumanReadable Fneq = "  Fneq\n"
toHumanReadable Igt = " Igt\n"
toHumanReadable Fgt = " Fgt\n"
toHumanReadable Ilt = " Ilt\n"
toHumanReadable Flt = " Flt\n"
toHumanReadable Ige = " Ige\n"
toHumanReadable Fge = " Fge\n"
toHumanReadable Ile = " Ile\n"
toHumanReadable Fle = " Fle\n"
toHumanReadable Iand = "  Iand\n"
toHumanReadable Ior = " Ior\n"
toHumanReadable Ixor = "  Ixor\n"
toHumanReadable (Ift _ b) = " Ift " ++ show b ++ "\n"
toHumanReadable (Iff _ b) = " Iff " ++ show b ++ "\n"
toHumanReadable (Goto _ b) = "  Goto " ++ show b ++ "\n"
toHumanReadable (Invoke _ b) = "  Invoke " ++ show b ++ "\n"
toHumanReadable Return = "  Return\n"
toHumanReadable I2f = " I2f\n"
toHumanReadable F2i = " F2i\n"
toHumanReadable NOTHING = "\n"

bytecode :: [Bytecode]
bytecode =
    [
      Funk 1 26,
      Iload 4 0,
      Iload 4 1,
      Iadd,
      Istore 4 2,
      Iload 4 2,
      Return,
      Iconst 4 5,
      Iconst 4 6,
      Invoke 1 0
    ]

getFloatFromHex :: [Char] -> Float
getFloatFromHex [] = 0
getFloatFromHex f = getFloatFromBytes $ concatMap (toBinary . digitToInt) f

getFloatFromBytes :: [Char] -> Float
getFloatFromBytes [] = 0
getFloatFromBytes f = getSignFromBytes f * getMantissaFromBytes f * getExponentFromBytes f

getSignFromBytes  :: [Char] -> Float
getSignFromBytes f
  | head f == '0' = 1
  | otherwise = -1

getMantissaFromBytes :: [Char] -> Float
getMantissaFromBytes f = 1 + c
  where c = foldl (\acc x -> acc / 2 + if x == '1' then 1 else 0) 0 (drop 9 f)

getExponentFromBytes :: [Char] -> Float
getExponentFromBytes f = 2 ^ (e - 127)
  where e = foldl (\acc x -> acc * 2 + digitToInt x) 0 (take 8 (tail f))