#!/bin/bash
set -xe
export HEAP_SIZE=$((0xfffff8))
export CHEZSCHEMELIBDIRS=".:./lib/"
PORT=22224
make rebuild-clean
make new-compiler \
    SCM_CC="./compiler-x86_64.out" \
    TARGET_ARCH=rv64 SCM_CC_BACKEND=compiler-riscv64.scm \
    SCM_NCC="compiler-riscv64.x86_64.out" -j &&\
export CC=riscv64-linux-gnu-gcc &&\
export OBJCOPY=riscv64-linux-gnu-objcopy &&\
make new-compiler \
    fLINK_RUNTIME=0 \
    SCM_CC="./compiler-riscv64.x86_64.out" \
    TARGET_ARCH=rv64 SCM_CC_BACKEND=compiler-riscv64.scm \
    SCM_NCC="compiler-riscv.o" -j &&\
rsync -avz -e "ssh -p ${PORT}" --exclude "*.out" --exclude "qemu-env" --exclude ".git" . root@localhost:~/scheme-compiler &&\
ssh -p ${PORT} root@localhost "\
    cd ~/scheme-compiler/ &&\
    source activate.sh &&\
    time gcc -c runtime.c &&\
    time gcc runtime.o compiler-riscv.o -o compiler-riscv64.out &&\
    time make tests TARGET_ARCH=rv64 SCM_CC="./compiler-riscv64.out" -j &&\
    time make bootstrap-test TARGET_ARCH=rv64 SCM_CC_BACKEND="compiler-riscv64.scm" SCM_CC="./compiler-riscv64.out" -j" &&\
rsync -avz -e "ssh -p ${PORT}" root@localhost:~/scheme-compiler/compiler-riscv64.out . &&\
exit 0