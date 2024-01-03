{-# LANGUAGE MultiParamTypeClasses #-}

module OpNumber (
  addI,
  addF,
  subI,
  subF,
  mulI,
  mulF,
  divI,
  divF,
  modI,
) where

import Control.Applicative (Alternative (..))
import Control.Monad.State
import Data.Int
import Data.Sequence as S
import Data.Word
import LexerVm
  ( Variable (..),
  )
import System.Environment (getArgs)

lenVar :: Variable -> Int
lenVar (I8 _) = 1
lenVar (I16 _) = 2
lenVar (I32 _) = 4
lenVar (I64 _) = 8
lenVar (F32 _) = 4
lenVar (F64 _) = 8
lenVar (ISize _) = 8
lenVar (U8 _) = 1
lenVar (U16 _) = 2
lenVar (U32 _) = 4
lenVar (U64 _) = 8
lenVar (USize _) = 8
lenVar (Bool _) = 1
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
getInt (ISize x) = x
getInt (USize x) = fromIntegral x
getInt (Bool x) = fromEnum x
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
getFloat (ISize x) = fromIntegral x
getFloat (USize x) = fromIntegral x
getFloat (Bool x) = fromIntegral $ fromEnum x
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
getDouble (ISize x) = fromIntegral x
getDouble (USize x) = fromIntegral x
getDouble (Bool x) = fromIntegral $ fromEnum x
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
convertVar (ISize _) (ISize x) = ISize x
convertVar (ISize _) other = ISize (getInt other)
convertVar (USize _) (USize x) = USize x
convertVar (USize _) other = USize (fromIntegral $ getInt other)
convertVar (Bool _) (Bool x) = Bool x
convertVar (Bool _) other = Bool (getInt other /= 0)
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
addI (ISize x) (ISize y) = ISize (x + y)
addI (USize x) (USize y) = USize (x + y)
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
subI (ISize x) (ISize y) = ISize (x - y)
subI (USize x) (USize y) = USize (x - y)
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
mulI (ISize x) (ISize y) = ISize (x * y)
mulI (USize x) (USize y) = USize (x * y)
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
divI (ISize x) (ISize y) = ISize (x `div` y)
divI (USize x) (USize y) = USize (x `div` y)
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
modI (ISize x) (ISize y) = ISize (x `mod` y)
modI (USize x) (USize y) = USize (x `mod` y)
modI a b =
  if lenVar a >= lenVar b
    then convertVar a (modI a (convertVar a b))
    else convertVar b (modI (convertVar b a) b)


