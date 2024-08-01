# About
This repo contains the compiler that compile scheme to i386 (or 32 bits of x86_64) machine code. The compiler source code is written scheme. The compiler can compile the compiler source code whose resulted compiler can again compile the compiler source code to get, say, `compiler-compiler.out` executable. The `compiler-compiler.out` manages to pass all the written tests.

# Prerequisite to run the compiler
- x86_64
- Linux OS
- installed gcc (default only install 64bits)
- installed 32bits gcc compiler for x86_64
- installed chez scheme
  - if you want to run the test
  - if you want to use scheme implementation other than the `compiler.out`

# How to compile
```bash
# run these 2 lines on startup
./compiler.out recompile-runtime
./compiler.out recompile-top-prog

./compiler.out ./test/test-unary.scm ./a.out link-with-top-prog
./a.out
```

# Run the test
Prerequisite: installed chez scheme
```bash
./compiler.out ./run-test.scm ./run-test.out link-with-top-prog
./run-test.out -1 # to run matryoshka test for at most 3
```

# Features used/supported
- pair-based pattern matcher
- Garbage collector
- C-FFI
- fixnum, ascii characters, boolean, vector, stirng, symbol, closure
- assignment
- tail call
- seperate compilation
- primitives are value procedures when occured in operand position

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
+--------+----+----+-----+-------+
| length | c0 | c1 | ... | #\nul |
+--------+----+----+-----+-------+
```
The extra null character is convenient in casting to C-string when interfacing with C functions.

## Vector Representation
```
+--------+--------+--------+--------+
| length |   v0   |   v1   |   ...  |
+--------+--------+--------+--------+
```

## Closure Representation
```
+------------+------------+------------+------------+------------+
|code pointer|   length   |     fv1    |    fv2     |     ...    |
+------------+------------+------------+------------+------------+
```

# Platform Dependent Implementation
- C: `getpid` is available on *nix system.

# Reference
- https://srfi.schemers.org/srfi-200/srfi-200.html
- http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf
- https://generalproblem.net/
- https://cplusplus.com/reference/cstdio/
- https://www.lispworks.com/documentation/lw50/CLHS/Body/f_format.htm