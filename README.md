# About
This repository contains a Scheme implementation written in Scheme itself.
It features a compiler that translates Scheme code into native machine code, currently supporting x86, AMD64 and AArch64 architectures.

The compiler is capable of bootstrapping - meaning it can compile its own source code and produce a fully functional binary, provided `gcc` and `glibc` are available.

Once bootstrapped, the resulting binary (referred to as `compiler-0.out`) can successfully recompile the compiler's source code and passes all included test cases. Additionally, the compiler supports cross-compilation, although it does not link against the C standard library during this process.

For example, in order to get arm64 compiler from amd64:
1. On an amd64 machine, use `compiler-amd64.out` to compile the compiler source with the ARM64 backend, producing `compiler-arm64-amd64.out`.
2. Still on the amd64 machine, use `compiler-arm64-amd64.out` to compile the same source again, resulting in `compiler-arm64.o`.
3. On a arm64 machine, compile and link the output:  
   `gcc runtime.c compiler-arm64.o -o compiler-arm64.out`
4. You now have a native arm64 compiler (`compiler-arm64.out`) and a cross-compiler (`compiler-arm64-amd64.out`) targeting arm64 from amd64.

For more details, see the [CHANGELOG](CHANGELOG.md).

# Prerequisite to run the compiler
- x86-64/AMD64 CPU or ARM64 CPU
- Ubuntu, Fedora or Fedora WSL, https://docs.fedoraproject.org/en-US/cloud/wsl/
- Installing dependecies (refer to dockerfile)

# How to use
```bash
source env.sh
make make_runtime TARGET_ARCH=amd64
make lib.o LIB=lib.o TARGET_ARCH=amd64 SCM_CC="./compiler-amd64.out" -j
# to run
./compiler-amd64.out lib.o test/test-utf8-read.scm
# to compile and run
./compiler-amd64.out -o a.out lib.o test/test-utf8-read.scm
./a.out
```

# Features used/supported
- pair-based pattern matcher
- Garbage collector
- C-FFI
- fixnum, unicode, boolean, vector, bytevector, symbol, closure
- assignment
- tail call
- seperate compilation
- primitives are value procedures when occurs in operand position
- variadic procedure, primitive case lambda, apply, tail call apply
- first-class continuation, `call/cc`
- multiple-values, `values`
- dynamic eval, `eval`

# Naming Convention for scheme compiler
- `compiler-<host=target>.out` - Runs on and generates code for the same architecture.
- `compiler-i686.out` - Targets the 32-bit variant of the x86-64 architecture (or i686).
- `compiler-amd64.out` - Targets the amd64 architecture (or x86-64).
- `compiler-arm64.out` - ditto

# Reference

## Compiler
- [An Incremental Approach to Compiler Construction](https://www.schemeworkshop.org/2006/11-ghuloum.pdf)
- [A Nanopass Framework for Compiler Education](https://legacy.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf)
- [Scheme Workshop Keynote: Andy Keep](https://www.youtube.com/watch?v=BcC3KScZ-yA)
- [Workshop: Mixing Mutability into the Nanopass Framework](https://www.youtube.com/watch?v=wTGlKCfP90A)
- [Scheme Style Guide](http://community.schemewiki.org/?scheme-style)
- [Scheme Variable Name Convention](http://community.schemewiki.org/?variable-naming-convention)
- [Rabbit: a compiler for scheme](https://dspace.mit.edu/handle/1721.1/6913)
- [Fixing Letrec](https://legacy.cs.indiana.edu/~dyb/pubs/fixing-letrec.pdf)
- [Fixing Letrec (reloaded)](https://legacy.cs.indiana.edu/~dyb/pubs/letrec-reloaded.pdf)
- [Optimizing Closures in O(0)-time](https://www.schemeworkshop.org/2012/papers/keep-hearn-dybvig-paper-sfp12.pdf)
- [Representing control in the presence of first-class continuations](https://dl.acm.org/doi/10.1145/93548.93554)
- [An efficient implementation of multiple return values in Scheme](https://dl.acm.org/doi/10.1145/182590.156784)

## Toolchain, ISA, and ABI
- [Compiler Explorer](https://godbolt.org/)
- [GNU assembler `as`](https://sourceware.org/binutils/docs/as/)
- [List of ABI](https://github.com/rui314/psabi)
- [x86 ISA Reference](https://c9x.me/x86)
- [Félix Cloutier's x86 and amd64 ISA Reference](https://www.felixcloutier.com/x86/)
- [Learn the architecture - A64 Instruction Set Architecture Guide](https://developer.arm.com/documentation/102374/latest/)
- [Arm A-profile A64 Instruction Set Architecture](https://developer.arm.com/documentation/ddi0602/2025-09/?lang=en)
- [Procedure Call Standard for the Arm® 64-bit Architecture (AArch64) 2025Q1](https://github.com/ARM-software/abi-aa/releases/download/2025Q1/aapcs64.pdf)
- [System V ABI for the Arm® 64-bit Architecture (AArch64) 2025Q1](https://github.com/ARM-software/abi-aa/releases/download/2025Q1/sysvabi64.pdf)
