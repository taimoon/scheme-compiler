# About
This repository contains a Scheme implementation written in Scheme itself.
It features a compiler that translates Scheme code into native machine code, currently supporting x64, x86, AArch64, and RISC-V64 architectures.

The compiler is capable of bootstrapping - meaning it can compile its own source code and produce a fully functional binary, provided `gcc` and `glibc` are available.

Once bootstrapped, the resulting binary (referred to as `compiler-0.out`) can successfully recompile the compiler's source code and passes all included test cases. Additionally, the compiler supports cross-compilation, although it does not link against the C standard library during this process.

For example, in order to get riscv64 compiler from x64:
1. On an x64 machine, use `compiler-x86_64.out` to compile the compiler source with the RISC-V64 backend, producing `compiler-riscv64.x86_64.out`.
2. Still on the x64 machine, use `compiler-riscv64.x86_64.out` to compile the same source again, resulting in `compiler-riscv.o`.
3. On a RISC-V64 machine, compile and link the output:  
   `gcc runtime.c compiler-riscv.o -o compiler-riscv64.out`
4. You now have a native RISC-V64 compiler (`compiler-riscv64.out`) and a cross-compiler (`compiler-riscv64.x86_64.out`) targeting RISC-V64 from x64.

For more details, see the [CHANGELOG](CHANGELOG).

# Prerequisite to run the compiler
- x86_64 CPU
- Fedora OS, or Fedora WSL, https://docs.fedoraproject.org/en-US/cloud/wsl/
- Installing dependecies (refer to dockerfile)

# How to use
```bash
source activate.sh
# Compile lib64.o and runtime.o
make make-lib runtime.o TARGET_ARCH=x86-64 SCM_CC="./compiler-x86_64.out" -j
# to run
./compiler-x86_64.out lib64.o test/test-unary.scm
# to compile and run
./compiler-x86_64.out -o a.out lib64.o test/test-unary.scm
./a.out
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

# Naming Convention for scheme compiler
- `compiler-<host=target>.out` – Runs on and generates code for the same architecture.
- `compiler-i686.out` – Targets the 32-bit variant of the x86_64 architecture.
- `compiler-x86_64.out` – Targets the 64-bit variant of the x86_64 architecture.
- `compiler-arm64.out`  - ditto
- `compiler-riscv64.out` - ditto

# Platform Dependent Implementation
- C: `getpid` is available on *nix system.
- C: `getenv` is available on *nix system.

# Reference
- [Compiler Explorer](https://godbolt.org/)
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