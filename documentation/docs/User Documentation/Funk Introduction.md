## The Funk Programming Language

Welcome to *The Funk Programming Language*, an introduction about Funk. Funk is a High-level and low-level control inspired from Rust and the C heritage. Funk is all about elegant syntax from modern language while providing control over your code.

## Who is Funk for ?

Funk is for programmers that are in a word: funky.
We here at the Funk Foundation, wanted a language that would enable people from all horizon and backgrounds to embrace their **true self**. Being a Funk Programmer is not just something to put on a resume, it is a whole spirit.

## Legal

Funk is an open-source language and is open to new contributor. The only requirement to write code for Funk is to be funky.

## Getting Started

Let's start your Funk journey! There's a lot to learn, but every journey starts somewhere. We'll discuss writing a *Hello World!* program in Funk and how to use the syntax.

### Hello World

Hello World! The classical, the first program you write everytime you learn something new. We'll do the same here in Funk!

Start by creating a new project folder and a file named `hello.fk`. Funk utilizes the `.fk` extension for its source files.

Start by creating a variable using the `var` keyword. Give it a name and then a type using the `:` symbol. Funk has multime primitive types we'll cover in a future chapter, for now we want to store a string. We'll use `u8[]` to store our string. Then add a value using the `=` symbol. We specify our message here. End your instructions with the required `;` semicolon and there you go you just created your first variable!

```ts
var hello: u8[] = "Hello World!";
```

Now in order to print it, we'll use the standard library for funk: `funkystd`.

At the top of your file, add the following `#include "funkystd"`. This will provide your program with more function utility than ever! We can now use the `putstr` function from the library to print our message.

```ts
#include "funkystd"

var hello: u8[] = "Hello World!";

putstrln(hello);
```

Congratulations! You just wrote your first program in Funk! But wait a minute, we still have not run it! Don't worry we'll see how to run Funk programs in the next chapter.

### Running Funk programs

You just made your first Funk *Hello World!* but you still have no clue how to run it? In this chapter we'll cover Funk execution.

Funk is a **compiled** language, meaning it needs to be put inside another program in order to execute it. But we got you covered, you won't have to write your own. Using your command-line interface, enter the following:

```console
$ funkc hello.fk
Created: out.bin
```

You get yourself a new file: `out.bin`. This file represent our program in **bytecode**. Like other compiled language, Funk is first compiled down to bytecode containing the set of instruction our program needs to make to achieve the wanted result.

This file is not an executable binary, it is bytecode. Now we can use **FVM** (Funk Virtual Machine) to execute the bytecode instructions. Think about it like Java execution. You compile your project into a `.jar` file containing the bytecode and then feed it to the **JVM** (Java Virtual Machine). This is not plagiarism, it is inspiration.

```console
$ fvm out.bin
Hello World!
```

And now its running! Your first program, is now displaying output on your terminal.
