# Changelog
## 2025-05-23
- Support AArch64 and RISC-V64
- Add new special form `foreign-call!` which uses the allocation pointer
- Hash Table implementation for symbol interning
- Revert `gensym` optimization

Emulating a foreign CPU architecture is always slower than running code natively, even on a less powerful processor.
For instance, the Raspberry Pi 400 significantly outperforms QEMU running aarch64 emulation.

Special thanks to my colleague TC sponsoring me the use of Raspberry Pi 400.
It is my first exposure to a computer that is between micro-controller and PC.
I use it for aarch64 porting and testing my compiler.
It is exciting to see my compiler can bootstrap itself on a modest machine.

I recently added new backends to the compiler: RISC-V64 and AArch64.  
The compiler’s architecture is divided into a front-end and thin back-end passes,
with many of these passes shared across different CPU targets.  
If you're okay with a "make it work" approach and not focused on optimization and reliability,
adding a new backend is easy - it took me about five man-days to complete.

This ease of backend implementation was made possible by adopting ANF IR.
Though other IRs such as SSA, three-address code, or CPS could also work.
ANF simplifies subsequent compiler passes.
To appreciate the advantage of ANF, consider how things look when working directly with the AST.

```scm
(+ ,e1 ,e2)
(emit-expr e1 si)
(emit "mov %rax, ~a(%rsp)" si)  ; store intermediate result
(emit-expr e2 (- si WORDSIZE))  ; avoid overwriting previous result
(emit "add ~a(%rsp), %rax")     ; use the stored value
```

This pattern is necessary to avoid clobbering intermediate values,
which could lead to incorrect code generation or misbehavior during garbage collection if memory is improperly scanned.

For RISC architectures, the above becomes more cumbersome due to stricter memory access rules,
making backend development more error-prone and labor-intensive.

However, once expressions are transformed into ANF,
we can safely assume operands are always values
and simplifying code generation:

```scm
(+ ,e1 ,e2)
(emit-simple e1 '%rax)
(emit-simple e2 '%rcx)
(emit "add %rcx, %rax")
```

ANF removes much of the manual bookkeeping required in traditional AST-based code generation.

## 2025-04-30
- Support x86-64 target
- Cross compilation between x86 and x64
- ANF IR and naive linear temporary allocation
- Inline GC check
- allocation pointer is stored in register
- Faster `gensym`
- Simple known call optimization

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