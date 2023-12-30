module Bytecode where

import Ast (Ast (..))

data Bytecode
    = Iload Int Int -- load int from stack
    | Fload Int Int -- load float from stack
    | Uload Int Int -- load uint from stack
    | Istore Int Int -- store int to stack
    | Fstore Int Int -- store float to stack
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
    | Ift Int -- if true jump of int bytes
    | Iff Int -- if false jump of int bytes
    | Goto Int -- jump of int bytes
    | Invoke Int -- invoke function at int index
    | Return -- return from function
    | I2f -- convert int to float
    | F2i -- convert float to int
