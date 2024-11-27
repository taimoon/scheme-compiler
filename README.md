# About
This repo contains the compiler that compile scheme to i386 (or 32 bits of x86_64) machine code. The compiler source code is written scheme. The compiler can compile the compiler source code whose resulted compiler can again compile the compiler source code to get, say, `compiler-compiler.out` executable. The `compiler-compiler.out` manages to pass all the written tests.

# Changelog
I complete rewrote the compiler.

Previous compiler ran slow in bootstrapping is due to missing a key pass in compilation.
One pass added is to lift the symbol interning to the top.
Otherwise, previous compiler interns every symbol occur in program.
When this pass added, the compiler can bootstrap within few seconds.
As comparison, previous compiler take at least 30 seconds.

I rework the linking process. The compiler use `objcopy` command to dump custom linking information into the section `.main_entry`.
When linking to executable, the compiler will read the info from the section `.main_entry` for the next step.

I experiment the calling convention not using `ebp` register as frame pointer
but the frame address is pushed by caller to the same position (after the return address).
Since `ebp` is free, I use it as closure pointer.

I've made the tail apply work properly. In addition, I add primitive case lambda.

# Prerequisite to run the compiler
- x86_64
- Linux OS
- installed gcc (default only install 64bits)
- installed 32bits gcc compiler for x86_64
- installed chez scheme
  - if you want to run the test
  - if you want to use scheme implementation other than the `compiler.out`

# How to run in docker

For non linux users, you may install docker for your OS,
then proceed to do development in docker environment.

Naively, think of `dockerfile` is a program of installation instruction for `docker` to make a `image`.
Once the image is built by `docker`, you may ask `docker` to load and run the `image`.
One of the option is to attach a terminal session after `docker` loading the `image`.
The terminal session is your linux dev environment.

```bash
docker build -t fedora-chez-scheme .
docker run -it -v $(pwd):/home/scheme-compiler fedora-chez-scheme
# now you have a simple linux dev environment
# may proceed to `How to compile`
```

# How to compile
```bash
source activate.sh
gcc -fno-omit-frame-pointer -m32 runtime.c -c -o runtime.o
# run these 2 lines to precompile the library
./compiler.out --make-prim-lib primitives.scm
./compiler.out --combine lib.o \
  intern.scm kernel.scm primitives.scm \
  lib/scheme-libs.scm lib/writer.scm lib/reader.scm

./compiler.out -o a.out lib.o test/test-unary.scm
./a.out
```

# Run the test
Prerequisite: installed chez scheme
```bash
./compiler.out -o run-test.out lib.o run-test.scm
./run-test.out
```

# Features used/supported
- pair-based pattern matcher
- Garbage collector
- C-FFI
- fixnum, ascii characters, boolean, vector, string, symbol, closure
- assignment
- tail call
- seperate compilation
- primitives are value procedures when occurs in operand position
- variadic procedure, primitive case lambda, apply, tail call apply

# Data Representation
## Memory Layout
```
      type      | 31            bit             0
-------------------------------------------------
        integer | iiiiiiiiiiiiiiiiiiiiiiiiiiiii00
           char | 000000000000000cccccccc00000111
        boolean | 0000000000000000000000b00001111
   pair pointer | pppppppppppppppppppppppppppp001
 vector pointer | pppppppppppppppppppppppppppp010
 string pointer | pppppppppppppppppppppppppppp011
 symbol pointer | pppppppppppppppppppppppppppp101
closure pointer | pppppppppppppppppppppppppppp110
```

## String Representation
```
+----------+----+----+-----+-------+
| fxlength | c0 | c1 | ... | #\nul |
+----------+----+----+-----+-------+
```
The extra null character is convenient in casting to C-string when interfacing with C functions.

## Vector Representation
```
+----------+--------+--------+--------+
| fxlength |   v0   |   v1   |   ...  |
+----------+--------+--------+--------+
```

## Closure Representation
```
+------------+------------+------------+------------+------------+
|code pointer|  fxlength  |     fv1    |    fv2     |     ...    |
+------------+------------+------------+------------+------------+
```

# Platform Dependent Implementation
- C: `getpid` is available on *nix system.

# Reference
- [SRFI 200: Pattern Matching](https://srfi.schemers.org/srfi-200/srfi-200.html)
- [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf)
- https://generalproblem.net/
- https://cplusplus.com/reference/cstdio/
- [how to extract data from elf files](https://stackoverflow.com/questions/1685483/how-can-i-examine-contents-of-a-data-section-of-an-elf-file-on-linux)
- [GNU assembler `as`](https://sourceware.org/binutils/docs/as/)
- [Scheme Variable Name Convention](http://community.schemewiki.org/?variable-naming-convention)
- [Workshop: Mixing Mutability into the Nanopass Framework](https://www.youtube.com/watch?v=wTGlKCfP90A)
- [x86 ISA Reference](https://c9x.me/x86)
- [Félix Cloutier's x86 ISA Reference](https://www.felixcloutier.com/x86/)