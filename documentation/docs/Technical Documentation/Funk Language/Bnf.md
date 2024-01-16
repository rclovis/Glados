#Grammar Rules

1.Program Structure

```bnf
<program> ::= { <declaration> }*
```

2.Declarations:
```bnf
<declaration> ::= <function-definition> | <pointer-definition> | <import-statement>
```

3.Function Definition:
```bnf
<function-definition> ::= "funk" <function-name> "(" [ <parameter-list> ] ")" ":" <return-type> "{" <statement>* "}"
```

4.Parameter List:
```bnf
<parameter-list> ::= <parameter> { "," <parameter> }*
```

5.Parameter:
```bnf
<parameter> ::= <variable-name> ":" <data-type>
```

6.Pointer Definition:
```bnf
<pointer-definition> ::= "var" <variable-name> ":" "*" <data-type> "=" "&" <variable-name> ";"
```

7.Import Statement:
```bnf
<import-statement> ::= "include" <string-literal> ";"
```

8.Expressions:
```bnf
<expression> ::= <variable-name> | <literal> | <binary-operation> | <unary-operation> | "(" <expression> ")"
```

9.Binary Operation:
```bnf
<binary-operation> ::= <expression> <binary-operator> <expression>
```

10.Unary Operation:
```bnf
<unary-operation> ::= <unary-operator> <expression>
```

11.Variables:
```bnf
<pointer> ::= "*" <variable-name>
```

12.Pointers:
```bnf
<pointer> ::= "*" <variable-name>
```

13.Data Types:
```bnf
<data-type> ::= "i8" | "u8" | "i16" | "u16" | "i32" | "u32" | "i64" | "u64" | "f32" | "f64"
```