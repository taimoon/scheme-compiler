# About
This repository contains a compiler that translates Scheme code into x86 machine code
(specifically, 32-bit x86 or the 32-bit subset of x86_64).
The compiler itself is implemented in Scheme.

It supports self-hosting: it can compile its own source code.
The resulting binary (let's call it compiler-compiler.out) is fully functional and capable of recompiling the compiler source code again.
This bootstrapped compiler passes all provided test cases.

# Changelog
## 2025-04-08
- Use `make` to do compilation and testing
- Support for concurrent compilation and testing.
  The testing time is shorter as you can run test in parallel.
- Support `include`
- Dependency: Linux OS `/dev/shm`
- A proper table to store globals for garbage collection
- Added passes:
    - lift constant
    - lift symbol
    - constant-folding

## 2024-11-21
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
- Fedora OS
- installed gcc (default only install 64bits)
- installed 32bits gcc compiler for x86_64
- installed chez scheme
  - if you want to run the test
  - if you want to use scheme implementation other than the `compiler.out`

# How to compile
```bash
make lib.o runtime.o -j
# to run
./compiler.out lib.o test/test-unary.scm
# to compile and run
./compiler.out -o a.out lib.o test/test-unary.scm
./a.out
```

# Run the test

```bash
sh run-test.sh
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
- [List of ABI](https://github.com/rui314/psabi)
- [An Incremental Approach to Compiler Construction](https://www.schemeworkshop.org/2006/11-ghuloum.pdf)
- [A Nanopass Framework for Compiler Education](https://legacy.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf)
- [Scheme Workshop Keynote: Andy Keep](https://www.youtube.com/watch?v=BcC3KScZ-yA)
- [Workshop: Mixing Mutability into the Nanopass Framework](https://www.youtube.com/watch?v=wTGlKCfP90A)
- [x86 ISA Reference](https://c9x.me/x86)
- [Félix Cloutier's x86 ISA Reference](https://www.felixcloutier.com/x86/)
- [GNU assembler `as`](https://sourceware.org/binutils/docs/as/)
- [Scheme Style Guide](http://community.schemewiki.org/?scheme-style)
- [Scheme Variable Name Convention](http://community.schemewiki.org/?variable-naming-convention)
- [Rabbit: a compiler for scheme](https://dspace.mit.edu/handle/1721.1/6913)
- [Fixing Letrec](https://legacy.cs.indiana.edu/~dyb/pubs/fixing-letrec.pdf)
- [Fixing Letrec (reloaded)](https://legacy.cs.indiana.edu/~dyb/pubs/letrec-reloaded.pdf)
- [Optimizing Closures in O(0)-time](https://www.schemeworkshop.org/2012/papers/keep-hearn-dybvig-paper-sfp12.pdf)