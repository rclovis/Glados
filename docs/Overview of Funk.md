## Variables

```
var variable: i32 = 0;
var variable: i32;
```

| Length  | Signed  | Unsigned |
| ------- | ------- | -------- |
| 8-bit   | `i8`    | `u8`     |
| 16-bit  | `i16`   | `u16`    |
| 32-bit  | `i32`   | `u32`    |
| 64-bit  | `i64`   | `u64`    |
| 128-bit | `i128`  | `u128`   |
| arch    | `isize` | `usize`  |

## Functions
```
funk fonction (): i32
{
	var variable: i32 = 0;
	variable
}
```

## Pointers
```
var variable: i32 = 0;
var pointer: *i32 = &variable;
```

## Imports
```
#include "pathToFile"
```

## Array
```
var array: vector<i32> = {0, 2, 3};
array.push(3);
array.size();
```

```
var array: vector<i32> = {0, 2, 3};
array::push(3);
array::size();
```
