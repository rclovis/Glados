# Binary
The bytecode is composed of [[Glados/Instructions|Instructions]]. Each instruction is followed with one or no arguments.

To be able to evaluate the bytecode three important structures are required:
## Operand Stack
This stack is used to manage variables during the call of functions.
Variables are pushed onto it thanks to `iload`, `fload`, `iconst` or `fconst` and are retrieve thanks to `istore`, `fstore` or `return`.

> [!warning]
> Elements of this stack are consumed upon use.

## Funk env
This stack is used to store the position of every function already parsed in the bytecode.

## Variable Env
This stack is used to store the value of variable with their index.
This stack has multiple instances during the eval of the bytecode because every `invoke` create a local variable env.

> [!warning]
> Upon `invoke` call, the arguments for the function must be loaded in the variable env from the index 0 to index number of the arguments.


# Examples
## #1
```rust
funk add (a: i32, b: i32): i32
{
	var c: i32 = a + b;
	c
}
add(5, 6)
```

```
; Bytecode for the add function
funk_26            ; Declares a function of 26 bytes in 6 instructions
iload_4_0         ; Load argument a of size 4 bytes
iload_4_1         ; Load argument b of size 4 bytes
iadd               ; Add a and b
istore_4_2        ; Store the result in local variable 2 (c)
iload_4_2         ; Load the result
return             ; Return the result

; Bytecode for invoking the add function
iconst_5           ; Push constant 5 (argument for a)
iconst_6           ; Push constant 6 (argument for b)
invoke #0          ; Invoke add function
; The result is now on the stack
```

## #2
```rust
funk factorial(n: i32): i32
{
	if (n == 0 || n == 1) {
        1
    } else {
        var result: i32 = 1;
        var i: i32 = 2;
        while (i <= n) {
	        result = result * i;
	        i = i + 1;
        }
        result
    }	
}
factorial(5);
```

```
; Bytecode for the add function
funk_
iload_0
iconst_0
ieq
iff_2
iconst_1
return
iload_0
iconst_1
ieq
iff_2
iconst_1
return
iconst_1
istore_1           ; Store top of operand stack into local variable 1 (result)
iconst_2
istore_2           ; Store top of operand stack into local variable 2 (i)
iload_2
iload_0
ile
iff_9
iload_1
iload_2
imul
istore_1
iload_2
iconst_1
iadd
istore_2
goto_-8
iload_1
return


; Bytecode for invoking the factorial function
iconst_5           ; Push constant 5 (argument for a)
invoke #0          ; Invoke factorial function
; The result is now on the stack
```