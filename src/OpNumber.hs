{-# LANGUAGE MultiParamTypeClasses #-}

module OpNumber
  ( addI,
    addF,
    subI,
    subF,
    mulI,
    mulF,
    divI,
    divF,
    modI,
    eqI,
    eqF,
    neI,
    neF,
    ltI,
    ltF,
    gtI,
    gtF,
    leI,
    leF,
    geI,
    geF,
    andI,
    orI,
    xorI,
  )
where

import Data.Bits
import Data.Int
import LexerVm
  ( Variable (..),
  )

bTI8 :: Bool -> Int8
bTI8 True = 1
bTI8 False = 0

lenVar :: Variable -> Int
lenVar (I8 _) = 1
lenVar (I16 _) = 2
lenVar (I32 _) = 4
lenVar (I64 _) = 8
lenVar (F32 _) = 4
lenVar (F64 _) = 8
lenVar (U8 _) = 1
lenVar (U16 _) = 2
lenVar (U32 _) = 4
lenVar (U64 _) = 8
lenVar None = 0

getInt :: Variable -> Int
getInt (I8 x) = fromIntegral x
getInt (I16 x) = fromIntegral x
getInt (I32 x) = fromIntegral x
getInt (I64 x) = fromIntegral x
getInt (U8 x) = fromIntegral x
getInt (U16 x) = fromIntegral x
getInt (U32 x) = fromIntegral x
getInt (U64 x) = fromIntegral x
getInt (F32 x) = round x
getInt (F64 x) = round x
getInt None = 0

getFloat :: Variable -> Float
getFloat (F32 x) = x
getFloat (F64 x) = realToFrac x
getFloat (I8 x) = fromIntegral x
getFloat (I16 x) = fromIntegral x
getFloat (I32 x) = fromIntegral x
getFloat (I64 x) = fromIntegral x
getFloat (U8 x) = fromIntegral x
getFloat (U16 x) = fromIntegral x
getFloat (U32 x) = fromIntegral x
getFloat (U64 x) = fromIntegral x
getFloat None = 0

getDouble :: Variable -> Double
getDouble (F32 x) = realToFrac x
getDouble (F64 x) = x
getDouble (I8 x) = fromIntegral x
getDouble (I16 x) = fromIntegral x
getDouble (I32 x) = fromIntegral x
getDouble (I64 x) = fromIntegral x
getDouble (U8 x) = fromIntegral x
getDouble (U16 x) = fromIntegral x
getDouble (U32 x) = fromIntegral x
getDouble (U64 x) = fromIntegral x
getDouble None = 0


convertVar :: Variable -> Variable -> Variable
convertVar (I8 _) (I8 x) = I8 x
convertVar (I8 _) other = I8 (fromIntegral $ getInt other)
convertVar (I16 _) (I16 x) = I16 x
convertVar (I16 _) other = I16 (fromIntegral $ getInt other)
convertVar (I32 _) (I32 x) = I32 x
convertVar (I32 _) other = I32 (fromIntegral $ getInt other)
convertVar (I64 _) (I64 x) = I64 x
convertVar (I64 _) other = I64 (fromIntegral $ getInt other)
convertVar (U8 _) (U8 x) = U8 x
convertVar (U8 _) other = U8 (fromIntegral $ getInt other)
convertVar (U16 _) (U16 x) = U16 x
convertVar (U16 _) other = U16 (fromIntegral $ getInt other)
convertVar (U32 _) (U32 x) = U32 x
convertVar (U32 _) other = U32 (fromIntegral $ getInt other)
convertVar (U64 _) (U64 x) = U64 x
convertVar (U64 _) other = U64 (fromIntegral $ getInt other)
convertVar (F32 _) (F32 x) = F32 x
convertVar (F32 _) other = F32 (getFloat other)
convertVar (F64 _) (F64 x) = F64 x
convertVar (F64 _) other = F64 (getDouble other)
convertVar None _ = None

addI :: Variable -> Variable -> Variable
addI (I8 x) (I8 y) = I8 (x + y)
addI (I16 x) (I16 y) = I16 (x + y)
addI (I32 x) (I32 y) = I32 (x + y)
addI (I64 x) (I64 y) = I64 (x + y)
addI (U8 x) (U8 y) = U8 (x + y)
addI (U16 x) (U16 y) = U16 (x + y)
addI (U32 x) (U32 y) = U32 (x + y)
addI (U64 x) (U64 y) = U64 (x + y)
addI a b =
  if lenVar a >= lenVar b
    then convertVar a (addI a (convertVar a b))
    else convertVar b (addI (convertVar b a) b)

addF :: Variable -> Variable -> Variable
addF (F32 x) (F32 y) = F32 (x + y)
addF (F64 x) (F64 y) = F64 (x + y)
addF a b =
  if lenVar a >= lenVar b
    then convertVar a (addF a (convertVar a b))
    else convertVar b (addF (convertVar b a) b)

subI :: Variable -> Variable -> Variable
subI (I8 x) (I8 y) = I8 (x - y)
subI (I16 x) (I16 y) = I16 (x - y)
subI (I32 x) (I32 y) = I32 (x - y)
subI (I64 x) (I64 y) = I64 (x - y)
subI (U8 x) (U8 y) = U8 (x - y)
subI (U16 x) (U16 y) = U16 (x - y)
subI (U32 x) (U32 y) = U32 (x - y)
subI (U64 x) (U64 y) = U64 (x - y)
subI a b =
  if lenVar a >= lenVar b
    then convertVar a (subI a (convertVar a b))
    else convertVar b (subI (convertVar b a) b)

subF :: Variable -> Variable -> Variable
subF (F32 x) (F32 y) = F32 (x - y)
subF (F64 x) (F64 y) = F64 (x - y)
subF a b =
  if lenVar a >= lenVar b
    then convertVar a (subF a (convertVar a b))
    else convertVar b (subF (convertVar b a) b)

mulI :: Variable -> Variable -> Variable
mulI (I8 x) (I8 y) = I8 (x * y)
mulI (I16 x) (I16 y) = I16 (x * y)
mulI (I32 x) (I32 y) = I32 (x * y)
mulI (I64 x) (I64 y) = I64 (x * y)
mulI (U8 x) (U8 y) = U8 (x * y)
mulI (U16 x) (U16 y) = U16 (x * y)
mulI (U32 x) (U32 y) = U32 (x * y)
mulI (U64 x) (U64 y) = U64 (x * y)
mulI a b =
  if lenVar a >= lenVar b
    then convertVar a (mulI a (convertVar a b))
    else convertVar b (mulI (convertVar b a) b)

mulF :: Variable -> Variable -> Variable
mulF (F32 x) (F32 y) = F32 (x * y)
mulF (F64 x) (F64 y) = F64 (x * y)
mulF a b =
  if lenVar a >= lenVar b
    then convertVar a (mulF a (convertVar a b))
    else convertVar b (mulF (convertVar b a) b)

divI :: Variable -> Variable -> Variable
divI (I8 x) (I8 y) = I8 (x `div` y)
divI (I16 x) (I16 y) = I16 (x `div` y)
divI (I32 x) (I32 y) = I32 (x `div` y)
divI (I64 x) (I64 y) = I64 (x `div` y)
divI (U8 x) (U8 y) = U8 (x `div` y)
divI (U16 x) (U16 y) = U16 (x `div` y)
divI (U32 x) (U32 y) = U32 (x `div` y)
divI (U64 x) (U64 y) = U64 (x `div` y)
divI a b =
  if lenVar a >= lenVar b
    then convertVar a (divI a (convertVar a b))
    else convertVar b (divI (convertVar b a) b)

divF :: Variable -> Variable -> Variable
divF (F32 x) (F32 y) = F32 (x / y)
divF (F64 x) (F64 y) = F64 (x / y)
divF a b =
  if lenVar a >= lenVar b
    then convertVar a (divF a (convertVar a b))
    else convertVar b (divF (convertVar b a) b)

modI :: Variable -> Variable -> Variable
modI (I8 x) (I8 y) = I8 (x `mod` y)
modI (I16 x) (I16 y) = I16 (x `mod` y)
modI (I32 x) (I32 y) = I32 (x `mod` y)
modI (I64 x) (I64 y) = I64 (x `mod` y)
modI (U8 x) (U8 y) = U8 (x `mod` y)
modI (U16 x) (U16 y) = U16 (x `mod` y)
modI (U32 x) (U32 y) = U32 (x `mod` y)
modI (U64 x) (U64 y) = U64 (x `mod` y)
modI a b =
  if lenVar a >= lenVar b
    then convertVar a (modI a (convertVar a b))
    else convertVar b (modI (convertVar b a) b)

eqI :: Variable -> Variable -> Variable
eqI (I8 x) (I8 y) = I8 (bTI8(x == y))
eqI (I16 x) (I16 y) = I8 (bTI8(x == y))
eqI (I32 x) (I32 y) = I8 (bTI8(x == y))
eqI (I64 x) (I64 y) = I8 (bTI8(x == y))
eqI (U8 x) (U8 y) = I8 (bTI8(x == y))
eqI (U16 x) (U16 y) = I8 (bTI8(x == y))
eqI (U32 x) (U32 y) = I8 (bTI8(x == y))
eqI (U64 x) (U64 y) = I8 (bTI8(x == y))
eqI a b =
  if lenVar a >= lenVar b
    then convertVar a (eqI a (convertVar a b))
    else convertVar b (eqI (convertVar b a) b)

eqF :: Variable -> Variable -> Variable
eqF (F32 x) (F32 y) = I8 (bTI8(x == y))
eqF (F64 x) (F64 y) = I8 (bTI8(x == y))
eqF a b =
  if lenVar a >= lenVar b
    then convertVar a (eqF a (convertVar a b))
    else convertVar b (eqF (convertVar b a) b)

neI :: Variable -> Variable -> Variable
neI (I8 x) (I8 y) = I8 (bTI8(x /= y))
neI (I16 x) (I16 y) = I8 (bTI8(x /= y))
neI (I32 x) (I32 y) = I8 (bTI8(x /= y))
neI (I64 x) (I64 y) = I8 (bTI8(x /= y))
neI (U8 x) (U8 y) = I8 (bTI8(x /= y))
neI (U16 x) (U16 y) = I8 (bTI8(x /= y))
neI (U32 x) (U32 y) = I8 (bTI8(x /= y))
neI (U64 x) (U64 y) = I8 (bTI8(x /= y))
neI a b =
  if lenVar a >= lenVar b
    then convertVar a (neI a (convertVar a b))
    else convertVar b (neI (convertVar b a) b)

neF :: Variable -> Variable -> Variable
neF (F32 x) (F32 y) = I8 (bTI8(x /= y))
neF (F64 x) (F64 y) = I8 (bTI8(x /= y))
neF a b =
  if lenVar a >= lenVar b
    then convertVar a (neF a (convertVar a b))
    else convertVar b (neF (convertVar b a) b)

ltI :: Variable -> Variable -> Variable
ltI (I8 x) (I8 y) = I8 (bTI8(x < y))
ltI (I16 x) (I16 y) = I8 (bTI8(x < y))
ltI (I32 x) (I32 y) = I8 (bTI8(x < y))
ltI (I64 x) (I64 y) = I8 (bTI8(x < y))
ltI (U8 x) (U8 y) = I8 (bTI8(x < y))
ltI (U16 x) (U16 y) = I8 (bTI8(x < y))
ltI (U32 x) (U32 y) = I8 (bTI8(x < y))
ltI (U64 x) (U64 y) = I8 (bTI8(x < y))
ltI a b =
  if lenVar a >= lenVar b
    then convertVar a (ltI a (convertVar a b))
    else convertVar b (ltI (convertVar b a) b)

ltF :: Variable -> Variable -> Variable
ltF (F32 x) (F32 y) = I8 (bTI8(x < y))
ltF (F64 x) (F64 y) = I8 (bTI8(x < y))
ltF a b =
  if lenVar a >= lenVar b
    then convertVar a (ltF a (convertVar a b))
    else convertVar b (ltF (convertVar b a) b)

gtI :: Variable -> Variable -> Variable
gtI (I8 x) (I8 y) = I8 (bTI8(x > y))
gtI (I16 x) (I16 y) = I8 (bTI8(x > y))
gtI (I32 x) (I32 y) = I8 (bTI8(x > y))
gtI (I64 x) (I64 y) = I8 (bTI8(x > y))
gtI (U8 x) (U8 y) = I8 (bTI8(x > y))
gtI (U16 x) (U16 y) = I8 (bTI8(x > y))
gtI (U32 x) (U32 y) = I8 (bTI8(x > y))
gtI (U64 x) (U64 y) = I8 (bTI8(x > y))
gtI a b =
  if lenVar a >= lenVar b
    then convertVar a (gtI a (convertVar a b))
    else convertVar b (gtI (convertVar b a) b)

gtF :: Variable -> Variable -> Variable
gtF (F32 x) (F32 y) = I8 (bTI8(x > y))
gtF (F64 x) (F64 y) = I8 (bTI8(x > y))
gtF a b =
  if lenVar a >= lenVar b
    then convertVar a (gtF a (convertVar a b))
    else convertVar b (gtF (convertVar b a) b)

leI :: Variable -> Variable -> Variable
leI (I8 x) (I8 y) = I8 (bTI8(x <= y))
leI (I16 x) (I16 y) = I8 (bTI8(x <= y))
leI (I32 x) (I32 y) = I8 (bTI8(x <= y))
leI (I64 x) (I64 y) = I8 (bTI8(x <= y))
leI (U8 x) (U8 y) = I8 (bTI8(x <= y))
leI (U16 x) (U16 y) = I8 (bTI8(x <= y))
leI (U32 x) (U32 y) = I8 (bTI8(x <= y))
leI (U64 x) (U64 y) = I8 (bTI8(x <= y))
leI a b =
  if lenVar a >= lenVar b
    then convertVar a (leI a (convertVar a b))
    else convertVar b (leI (convertVar b a) b)

leF :: Variable -> Variable -> Variable
leF (F32 x) (F32 y) = I8 (bTI8(x <= y))
leF (F64 x) (F64 y) = I8 (bTI8(x <= y))
leF a b =
  if lenVar a >= lenVar b
    then convertVar a (leF a (convertVar a b))
    else convertVar b (leF (convertVar b a) b)

geI :: Variable -> Variable -> Variable
geI (I8 x) (I8 y) = I8 (bTI8(x >= y))
geI (I16 x) (I16 y) = I8 (bTI8(x >= y))
geI (I32 x) (I32 y) = I8 (bTI8(x >= y))
geI (I64 x) (I64 y) = I8 (bTI8(x >= y))
geI (U8 x) (U8 y) = I8 (bTI8(x >= y))
geI (U16 x) (U16 y) = I8 (bTI8(x >= y))
geI (U32 x) (U32 y) = I8 (bTI8(x >= y))
geI (U64 x) (U64 y) = I8 (bTI8(x >= y))
geI a b =
  if lenVar a >= lenVar b
    then convertVar a (geI a (convertVar a b))
    else convertVar b (geI (convertVar b a) b)

geF :: Variable -> Variable -> Variable
geF (F32 x) (F32 y) = I8 (bTI8(x >= y))
geF (F64 x) (F64 y) = I8 (bTI8(x >= y))
geF a b =
  if lenVar a >= lenVar b
    then convertVar a (geF a (convertVar a b))
    else convertVar b (geF (convertVar b a) b)

andI :: Variable -> Variable -> Variable
andI (I8 x) (I8 y) = I8 (x .&. y)
andI (I16 x) (I16 y) = I16 (x .&. y)
andI (I32 x) (I32 y) = I32 (x .&. y)
andI (I64 x) (I64 y) = I64 (x .&. y)
andI (U8 x) (U8 y) = U8 (x .&. y)
andI (U16 x) (U16 y) = U16 (x .&. y)
andI (U32 x) (U32 y) = U32 (x .&. y)
andI (U64 x) (U64 y) = U64 (x .&. y)
andI a b =
  if lenVar a >= lenVar b
    then convertVar a (andI a (convertVar a b))
    else convertVar b (andI (convertVar b a) b)

orI :: Variable -> Variable -> Variable
orI (I8 x) (I8 y) = I8 (x .|. y)
orI (I16 x) (I16 y) = I16 (x .|. y)
orI (I32 x) (I32 y) = I32 (x .|. y)
orI (I64 x) (I64 y) = I64 (x .|. y)
orI (U8 x) (U8 y) = U8 (x .|. y)
orI (U16 x) (U16 y) = U16 (x .|. y)
orI (U32 x) (U32 y) = U32 (x .|. y)
orI (U64 x) (U64 y) = U64 (x .|. y)
orI a b =
  if lenVar a >= lenVar b
    then convertVar a (orI a (convertVar a b))
    else convertVar b (orI (convertVar b a) b)

xorI :: Variable -> Variable -> Variable
xorI (I8 x) (I8 y) = I8 (x `xor` y)
xorI (I16 x) (I16 y) = I16 (x `xor` y)
xorI (I32 x) (I32 y) = I32 (x `xor` y)
xorI (I64 x) (I64 y) = I64 (x `xor` y)
xorI (U8 x) (U8 y) = U8 (x `xor` y)
xorI (U16 x) (U16 y) = U16 (x `xor` y)
xorI (U32 x) (U32 y) = U32 (x `xor` y)
xorI (U64 x) (U64 y) = U64 (x `xor` y)
xorI a b =
  if lenVar a >= lenVar b
    then convertVar a (xorI a (convertVar a b))
    else convertVar b (xorI (convertVar b a) b)


