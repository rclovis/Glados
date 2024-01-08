# Overview of Funk

## Variables

```c#
var variable: i32 = 0;
var variable: i32;
```

| Length  | Signed  | Unsigned | Float    |
| ------- | ------- | -------- | -------- |
| 8-bit   | `i8`    | `u8`     |          |
| 16-bit  | `i16`   | `u16`    |          |
| 32-bit  | `i32`   | `u32`    | `f32`    |
| 64-bit  | `i64`   | `u64`    | `f64`    |

## Functions

```c#
funk fonction (): i32
{
	var variable: i32 = 0;
	if (variable == 0)
	{
		return 0;
	}
	while (variable < 10)
	{
		variable += 1;
	}
	return variable;
}
fonction()


funk add (a: i32, b: i32): i32
{
	return a + b;
}
```

## Pointers

```c#
var variable: i32 = 0;
var pointer: *i32 = &variable;
```

## Imports
```
include "pathToFile"
```
