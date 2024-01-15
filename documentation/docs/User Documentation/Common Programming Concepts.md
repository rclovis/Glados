## Common Programming Concepts

In this chapter, we'll cover all the common concepts Funk shares with other languages. Specifically we'll talk variables, loop, conditional statements, functions and IO.

## Variables

Variables in Funk are similar to all the other variable concepts. It is an identifier used to store data in memory.

We use the `var` syntaxe to define variable and bind them to a type. Once a variable is bound it cannot be associated to data of another type unless we re-declare it again using `var` and specifying a type.

Here is how to use `var`:

```ts
var my_variable: i32 = 10;
```

This creates a new variable in the stack. We specify after the `:` a type. Types will be described in the next chapter. Then using equal `=` we associate an expression, here `10` to the variable. You may notice a semicolon `;` at the end of the line, all Funk instruction must end with a `;` to be valid.

Once a variable is defined you can re-assign a new value using the assign syntax and the `=` symbol.

```ts
var my_variable: i32 = 10;
my_variable = 20;
```

This will change the value stored inside the variable but not the size of the variable.

## Types

### Primitive types

Funk supports up to 64-bits numbers.

|Signed|Unsigned|Size|Description|
|-|-|-|-|
|i8|u8|8-bits|integer|
|i16|u16|16-bits|integer|
|i32|u32|32-bits|integer|
|i64|u64|64-bits|integer|
|f32||32-bit|float|
|f64||64-bits|float|

### Arrays

An array is a fixed size, list of items of the same type. In memory, each element in the array is next to the other. Every bit of the array is aligned in memory.

The type representation for a 8-bit unsigned array of 10 elements is as follows: `u8[10]`. In Funk we use the brakets to specify an array, and an litteral integer telling its size. the size of each element is given by prefixing the brackets with a primitive type.

In order to give it a value we use brackets and separate each expression by a comma.

```ts
var my_array: i32[5] = [10, 20, 30, 40, 50];
```

Funk also provides you with some syntactic sugar for array creation.

Leave the array type's brackets empty, and Funk will automatically infer the size using the given array value.

```ts
var my_array: i32[] = [10, 20, 30, 40, 50];
// Automatically generate an array of length 5
```

Regarding arrays that are longer than what your poor fingers can type, you can simply ignore what's left and it will be set to a default value (0)

```ts
var my_array: i32[100] = [10, 20, 30];
// That's a long one!
//Thankfully, Funk handles the 97 other values to put in the array, giving all those remaining values the default 0
```

### Strings

Strings in Funk are as in C, arrays of integer. However, you can specify the lenght of a character when creating a string, supporting other encoding format like 64-bit unicode.

You can define a string using doublequotes `""` as the value for a typical array. Funk supports escaped characters like `\n`, `\0`, `\t` and more.

```ts
var my_string: u8[] = "Hello!";
```

As with arrays, syntactic sugar is usable to infer the size of the string, but you can also manually set it or give a smaller string than what you're actually writing to give it a small buffer (it will be 0 initialized).

## Arithmetic Operators

Funk provides the usual operators for arithmetic operations.

|Symbol|Name|Symbol|Name|
|-|-|-|-|
|+|Addition|-|Substraction|
|*|Multiplication|/|Division|
|%|Modulus|||

## Logic operators

Funk provides the usual operators for handling logic.

|Symbol|Name|Symbol|Name|
|-|-|-|-|
|==|Equality|!=|Inequality|
|>|Greater than|>=|Greater or equal|
|<|Lesser than|<=|Lesser or equal|
|&&|And|\|\||Or|
|!|Not|

As in C these operators return `1` if *true* and `0` if *false*.

## Conditional statements

Conditional logic is done using the one and only `if` statement. Used alongside a condition in parentheses and a *then* scoped block, `if` can be used for conditional expressions.

```ts
if (x < 10) {
    return 0;
}
```

We can optionnaly specify an `else` statement in case the condition is `false`.

```ts
if (x < 10) {
    return 0;
} else {
    x = x - 1;
}
```

### Indexing

In order to access a certain element in a array (strings are arrays too), you can use the indexing syntax and specify an index to get. Simply put brackets after the name of the array you want to access.

**Remember:** in Funk, arrays start at 0.

```ts
var my_string: u8[] = "Hello!";
var letter_l: u8 = my_string[3];
```

## Iterating

Funk is an imperative programming language to its core, and so it needs iterating! To iterate, you can use the ultimate loop mankind ever created: the `while` loop. It functions as every other while loop before it, give it a condition and a block of instructions to repeat, and it will until the condition is *false*.

```ts
var counter: i32 = 10;

while (counter > 0) {
    counter = counter - 1;
}
```
