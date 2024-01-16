# Glados

The Glados project aims at developing a language from scratch with a compiler and a vm for it. The language is a simple imperative language, with a C-like syntax. The compiler and the vm are written in haskell.

## Prerequisites

Before you begin, ensure you have these packages installed in your machine:
- **Haskell**
- **Stack**

## Getting Started

### Coding In Funk
To learn how to code in Funk, you can read the [Funk Language Documentation](User%20Documentation/Funk%20Introduction.md).

### Running Funk
To run your code written in Funk, you must first compile it to bytecode. You can do so by running the following command:
```bash
stack run compile-exe [path to your files]
```
This will generate a `out.bin` file in the same directory as your source files.
To run the bytecode, you can use the following command:
```bash
stack run vm-exe [path to your bytecode] [arguments]
```

## Creators

<div style="display: flex; justify-content: space-between;">
    <a href="https://github.com/EstusSipper">
        <img src="https://avatars.githubusercontent.com/u/91874316?v=4" width="150" height="150">
        <p>Erwan Gonzales</p>
    </a>
    <a href="https://github.com/rclovis">
        <img src="https://avatars.githubusercontent.com/u/91875893?v=4" width="150" height="150">
        <p>Clovis Rabot</p>
    </a>
    <a href="https://github.com/TotoFunki">
        <img src="https://media.licdn.com/dms/image/D4E03AQF5p--YcDCWoQ/profile-displayphoto-shrink_800_800/0/1697809819173?e=1706745600&v=beta&t=FIEWA66x7PpYe9ZfTns7dk1rxuWL16BEAU34tHwqS0Q" width="150" height="150">
        <p>Théo Liennard</p>
    </a>
    <a href="https://github.com/AngeloZhou22">
        <img src="https://avatars.githubusercontent.com/u/91876442?s=400&u=e17541db376ba488505351104ee598772dbe67a2&v=4" width="150" height="150">
        <p>Angelo Zhou</p>
    </a>
</div>
