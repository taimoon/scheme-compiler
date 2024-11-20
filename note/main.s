/*
    how to run
    gcc -fomit-frame-pointer -m32 main.s -o main.o
*/
.section .data
    dfmt: .asciz "%p\n"
    x: .byte 0x13
.section .text
    .globl main
    .extern printf
main:
    // same as eax = *x
    movl x, %eax
    pushl %eax
    pushl $dfmt
    call printf
    addl $8, %esp
    
    // same as eax = *x
    movl (x), %eax
    pushl %eax
    pushl $dfmt
    call printf
    addl $8, %esp

    // same as eax = x
    movl $x, %eax
    pushl %eax
    pushl $dfmt
    call printf
    addl $8, %esp

    // same as eax = x
    leal x, %eax
    pushl %eax
    pushl $dfmt
    call printf
    addl $8, %esp

    // same as *x = eax
    movl $0x17, (x)
    movl x, %eax
    pushl %eax
    pushl $dfmt
    call printf
    addl $8, %esp


    // same as *x = eax
    movl $0x19, x
    movl x, %eax
    pushl %eax
    pushl $dfmt
    call printf
    addl $8, %esp

    movl $0, %eax
    ret
