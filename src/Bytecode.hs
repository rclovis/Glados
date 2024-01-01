module Bytecode ( toHexa, bytecode, getBinHexa, floatToHex) where

import Data.Char (digitToInt, intToDigit)

data Bytecode
    = Funk Int Int -- function with int size of arg and int size of body
    | Iload Int Int -- load int from stack
    | Fload Int Int -- load float from stack
    | Uload Int Int -- load uint from stack
    | Istore Int Int -- store int to stack
    | Fstore Int Int -- store float to stacktraverse toBinHexa xs
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

getBinHexa :: [Bytecode] -> [Char]
getBinHexa = concatMap toBinHexa

toBinHexa :: Bytecode -> [Char]
toBinHexa (Funk a b) = "00" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa (Iload a b) = "01" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa (Fload a b) = "02" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa (Uload a b) = "03" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa (Istore a b) = "04" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa (Fstore a b) = "05" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa (Ustore a b) = "06" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa (Iconst a b) = "07" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa (Fconst a b) = "08" ++ outputWell 1 a ++ floatToHex b ++ " " -- to do : float
toBinHexa (Uconst a b) = "09" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa Iadd = "0A "
toBinHexa Fadd = "0B "
toBinHexa Isub = "0C "
toBinHexa Fsub = "0D "
toBinHexa Imul = "0E "
toBinHexa Fmul = "0F "
toBinHexa Idiv = "10 "
toBinHexa Fdiv = "11 "
toBinHexa Imod = "12 "
toBinHexa Ieq = "13 "
toBinHexa Feq = "14 "
toBinHexa Ineq = "15 "
toBinHexa Fneq = "16 "
toBinHexa Igt = "17 "
toBinHexa Fgt = "18 "
toBinHexa Ilt = "19 "
toBinHexa Flt = "1A "
toBinHexa Ige = "1B "
toBinHexa Fge = "1C "
toBinHexa Ile = "1D "
toBinHexa Fle = "1E "
toBinHexa Iand = "1F "
toBinHexa Ior = "20 "
toBinHexa Ixor = "21 "
toBinHexa (Ift a b) = "22" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa (Iff a b) = "23" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa (Goto a b) = "24" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa (Invoke a b) = "25" ++ outputWell 1 a ++ outputWell a b ++ " "
toBinHexa Return = "26 "
toBinHexa I2f = "27 "
toBinHexa F2i = "28 "


-- toBinHexa :: Bytecode -> [Char]
-- toBinHexa (Funk a b) = "00" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa (Iload a b) = "01" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa (Fload a b) = "02" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa (Uload a b) = "03" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa (Istore a b) = "04" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa (Fstore a b) = "05" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa (Ustore a b) = "06" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa (Iconst a b) = "07" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa (Fconst a b) = "08" ++ outputWell 1 a ++ outputWell a (truncate b) -- to do : float
-- toBinHexa (Uconst a b) = "09" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa Iadd = "0A"
-- toBinHexa Fadd = "0B"
-- toBinHexa Isub = "0C"
-- toBinHexa Fsub = "0D"
-- toBinHexa Imul = "0E"
-- toBinHexa Fmul = "0F"
-- toBinHexa Idiv = "10"
-- toBinHexa Fdiv = "11"
-- toBinHexa Imod = "12"
-- toBinHexa Ieq = "13"
-- toBinHexa Feq = "14"
-- toBinHexa Ineq = "15"
-- toBinHexa Fneq = "16"
-- toBinHexa Igt = "17"
-- toBinHexa Fgt = "18"
-- toBinHexa Ilt = "19"
-- toBinHexa Flt = "1A"
-- toBinHexa Ige = "1B"
-- toBinHexa Fge = "1C"
-- toBinHexa Ile = "1D"
-- toBinHexa Fle = "1E"
-- toBinHexa Iand = "1F"
-- toBinHexa Ior = "20"
-- toBinHexa Ixor = "21"
-- toBinHexa (Ift a b) = "22" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa (Iff a b) = "23" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa (Goto a b) = "24" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa (Invoke a b) = "25" ++ outputWell 1 a ++ outputWell a b
-- toBinHexa Return = "26"
-- toBinHexa I2f = "27"
-- toBinHexa F2i = "28"

bytecode :: [Bytecode]
bytecode =
    [ Funk 1 26
    , Iconst 4 5
    , Fconst 4 6.5
    , Iadd
    , Return
    ]