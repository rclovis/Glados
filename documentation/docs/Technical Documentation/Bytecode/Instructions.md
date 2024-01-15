## Bytecode Instructions

1. **Memory Management Instructions:**
	- `iload`, `uload`, `fload`: Load an integer, unsigned integer or float from a local variable onto the operand stack. It takes the index and the size of the variable as arguments.
	- `istore`, `ustore`, `fstore`: Store an integer, unsigned integer or float at the top of the operand stack, into a local variable. It takes the index and the size of the variable as arguments.
	- `iconst`, `uconst`, `fconst`: Load an integer, unsigned integer or float from an argument onto the operand stack. It takes  and the size of the variable and the variable as arguments.
	- `funk`: Creates a function with a number of instructions, written in bytes (8bits) as argument
2. **Arithmetic Instructions:**
    - `iadd`, `fadd`: Add two (unsigned) integers or floats.
    - `isub`, `fsub`: Subtract two (unsigned) integers or floats.
    - `imul`, `fmul`: Multiply two (unsigned) integers or floats.
    - `idiv`, `fdiv`: Divide two (unsigned) integers or floats.
    - `imod`: Remainder after integer division.
3. **Comparison and Conditional Instructions:**
    - `ieq`, `ine`, `ilt`, `igt`, `ile`, `ige`: Compare two (unsigned) integers. (==, !=, <, >, <=, >=)
    - `feq`, `fne`, `flt`, `fgt`, `fle`, `fge`: Compare two floats. (==, !=, <, >, <=, >=)
    - `ift`: Conditional jumps if true, take the number of bytes to jump as argument.
    - `iff`: Conditional jumps if false, take the number of bytes to jump as argument.
    - `goto`: Unconditional jump, take the number of bytes to jump as argument.
4. **Logical Instructions:**
    - `iand`, `ior`, `ixor`: Bitwise AND, OR, XOR for integers.
5. **Control Flow Instructions:**
    - `invoke`: Invoke method.
    - `return`: Return from the current method.
6. **Type Conversion Instructions:**
    - `i2f`, `f2i`: Convert integer to float or float to integer.
7. **Stack Instructions:**
    - `pop`: Pop the top element from the operand stack.
    - `dup`: Duplicate the top element on the operand stack.
    - `popPrev`: Pop the element below the top element from the operand stack.
    - `iloadStack`, `floadStack`, `uloadStack`: Load an integer, unsigned integer or float from the operand stack onto the operand stack. It takes the index and the size of the variable as arguments.
8. **Bitwise Instructions:**
    - `not`: Bitwise NOT for integers.
9. **Type Conversion Instructions:**
    - `iconvert`, `uconvert`, `fconvert`: Convert integer to unsigned integer or float, unsigned integer to integer or float, float to integer or unsigned integer.
10. **Memory Access Instructions:**
    - `addr`: Get the address of a variable.
    - `access`: Access a variable at a given address.
    - `modify`: Modify a variable at a given address.
    - `write`: Write a value in the stdout.
    - `allocate`: Allocate a given number of bytes on the heap.
    - `getArg`: Get the argument at a given index.

| Bin Code      | Instruction                              | Argument      |
| ------------- | ---------------------------------------- | --------- |
| 0x00          | `funk`                                   | True          |
| 0x01 ... 0x03 | `iload`, `fload`, `uload`                | True          |
| 0x04 ... 0x06 | `istore`, `fstore`, `ustore`             | True          |
| 0x07 ... 0x09 | `iconst`, `fconst`, `uconst`             | True          |
| 0x0A, 0x0B    | `iadd`, `fadd`                           | False          |
| 0x0C, 0x0D    | `isub`, `fsub`                           | False          |
| 0x0E, 0x0F    | `imul`, `fmul`                           | False          |
| 0x10, 0x11    | `idiv`, `fdiv`                           | False          |
| 0x12          | `imod`                                   | False          |
| 0x13 ... 0x18 | `ieq`, `ine`, `ilt`, `igt`, `ile`, `ige` | False          |
| 0x19 ... 0x1E | `feq`, `fne`, `flt`, `fgt`, `fle`, `fge` | False          |
| 0x1F, 0x20    | `ift`, `iff`                             | True          |
| 0x21          | `goto`                                   | True          |
| 0x22 ... 0x24 | `iand`, `ior`, `ixor`                    | False         |
| 0x25          | `invoke`                                 | True          |
| 0x26          | `return`                                 | False          |
| 0x27, 0x28    | `i2f`, `f2i`                             | False          |
| 0x29          | `pop`                                    | True          |
| 0x2A          | `dup`                                    | True          |
| 0x2B          | `popPrev`                                | True          |
| 0x2C ... 0x2E | `iloadStack`, `floadStack`, `uloadStack` | True          |
| 0x2F          | `not`                                    | False          |
| 0x30 ... 0x32 | `iconvert`, `uconvert`, `fconvert`       | True          |
| 0x33          | `addr`                                   | True          |
| 0x34          | `access`                                 | False          |
| 0x35          | `modify`                                 | False          |
| 0x36          | `write`                                  | False          |
| 0x37          | `allocate`                               | False          |
| 0x38          | `getArg`                                 | False          |