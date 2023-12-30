1. **Memory Management Instructions:**
	- `iload`, `fload`: Load an integer, float from a local variable onto the operand stack.
	- `istore`, `fstore`: Store an integer, float at the top of the operand stack, into a local variable.
	- `iconst`, `fconst`: Load an integer, float from an argument onto the operand stack.
	- `funk`: Creates a function with a number of instructions given as arguments
2. **Arithmetic Instructions:**
    - `iadd`, `fadd`: Add two integers or floats.
    - `isub`, `fsub`: Subtract two integers or floats.
    - `imul`, `fmul`: Multiply two integers or floats.
    - `idiv`, `fdiv`: Divide two integers or floats.
    - `irem`: Remainder after integer division.
3. **Comparison and Conditional Instructions:**
    - `ieq`, `ine`, `ilt`, `igt`, `ile`, `ige`: Compare two integers.
    - `feq`, `fne`, `flt`, `fgt`, `fle`, `fge`: Compare two floats.
    - `ift`: Conditional jumps if true.
    - `iff`: Conditional jumps if false.
    - `goto`: Unconditional jump.
4. **Logical Instructions:**
    - `iand`, `ior`, `ixor`: Bitwise AND, OR, XOR for integers.
5. **Control Flow Instructions:**
    - `invoke`: Invoke method.
    - `return`: Return from the current method.
6. **Type Conversion Instructions:**
    - `i2f`, `f2i`: Convert integer to float or float to integer.

| Bin Code      | Instruction                              | Argument |
| ------------- | ---------------------------------------- | --------- |
| 0x00          | `funk`                                   | True          |
| 0x01, 0x02    | `iload`, `fload`                         | True          |
| 0x03, 0x04    | `istore`, `fstore`                       | True          |
| 0x05, 0x06    | `iconst`, `fconst`                       | True          |
| 0x07, 0x08    | `iadd`, `fadd`                           | False          |
| 0x09, 0x0A    | `isub`, `fsub`                           | False          |
| 0x0B, 0x0C    | `imul`, `fmul`                           | False          |
| 0x0D, 0x0E    | `idiv`, `fdiv`                           | False          |
| 0x0F          | `irem`                                   | False          |
| 0x10 ... 0x15 | `ieq`, `ine`, `ilt`, `igt`, `ile`, `ige` | False          |
| 0x16 ... 0x1B | `feq`, `fne`, `flt`, `fgt`, `fle`, `fge` | False          |
| 0x1C, 0x1D           | `ift`, `iff`                                     | True          |
| 0x1E          | `goto`                                   | True          |
| 0x1F ... 0x21 | `iand`, `ior`, `ixor`                    | False          |
| 0x22          | `invoke`                                 | True          |
| 0x23          | `return`                                 | False          |
| 0x24, 0x25    | `i2f`, `f2i`                             | False          |
