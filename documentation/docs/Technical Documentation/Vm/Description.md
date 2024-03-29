## Description

The Funk's VM composed of two parts:
- The lexer
- The execution engine

### Lexer
The lexer is the first part of the VM. It is responsible for parsing the source code and generating a list of tokens. Those tokens are then used by the execution engine to execute the logic of the program.

It allows the VM to be able to execute the logic of the program without having to parse the source code at runtime. This is a huge performance boost.

### Execution engine
The execution engine is the second part of the VM. It is responsible for executing the logic of the program. It takes the list of tokens generated by the lexer and executes the logic of the program.

It is composed of several parts:
- The stack
- The heap
- The Variable table
- The function table

#### Stack
The stack is the main data structure of the execution engine. It is used to store the values of the program.

#### Heap
The heap is the second data structure of the execution engine. It is used to store the values of the program that are too big to fit on the stack.

#### Variable table
The variable table is the third data structure of the execution engine. It is used to store the variables of the program.

#### Function table
The function table is the fourth data structure of the execution engine. It is used to store the functions of the program.
