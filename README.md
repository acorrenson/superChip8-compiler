# superChip8-compiler

An experimental assembler for [chip8](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM) language.

## Installing

This project requires ocaml and dune.

```
git clone https://github.com/jdrprod/superChip8-compiler.git
cd superChip8-compiler
dune install chip8asm
```

## Using

This assembler simply takes a `.src` file and outputs a `.rom` file. The command `chip8asm file.src` produces the binary file `file.rom`. Roms can then be executed using any working chip8 emulator. One is available [here](https://github.com/jdrprod/superChip8).



