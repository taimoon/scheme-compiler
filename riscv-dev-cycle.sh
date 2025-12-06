#!/bin/bash
set -xe
export HEAP_SIZE=$((0xfffff8))
export CHEZSCHEMELIBDIRS=".:./lib/"
make rebuild-clean
make new-compiler \
    SCM_CC="./compiler-x86_64.out" \
    TARGET_ARCH=rv64 SCM_CC_BACKEND=compiler-riscv64.scm \
    SCM_NCC="compiler-riscv64-x86_64.out" -j
CC=riscv64-linux-gnu-gcc OBJCOPY=riscv64-linux-gnu-objcopy make new-compiler \
    fLINK_RUNTIME=0 \
    SCM_CC="./compiler-riscv64-x86_64.out" \
    TARGET_ARCH=rv64 SCM_CC_BACKEND=compiler-riscv64.scm \
    SCM_NCC="compiler-riscv.o" -j
PORT=22224
HOST_NAME=root@localhost
rsync -avz -e "ssh -p ${PORT}" --exclude "*.out" --exclude "qemu-env" --exclude ".git" . ${HOST_NAME}:~/scheme-compiler
ssh -p ${PORT} ${HOST_NAME} "cd ~/scheme-compiler/ && TARGET_ARCH=riscv64 ./upgrade.sh"
rsync -avz -e "ssh -p ${PORT}" ${HOST_NAME}:~/scheme-compiler/compiler-riscv64.out .