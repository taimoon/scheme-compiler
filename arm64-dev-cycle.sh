#!/bin/bash
set -xe
export HEAP_SIZE=$((0xfffff8))
export CHEZSCHEMELIBDIRS=".:./lib/"
PORT=22223
make \
    new-compiler \
    SCM_CC="./compiler-x86_64.out" \
    TARGET_ARCH="armv8-a" \
    SCM_CC_BACKEND="compiler-arm64.scm" \
    SCM_NCC="./compiler-arm.x86_64.out" -j
export CC="aarch64-linux-gnu-gcc"
export OBJCOPY="aarch64-linux-gnu-objcopy"
make new-compiler \
    fLINK_RUNTIME=0 \
    SCM_CC="./compiler-arm.x86_64.out" \
    TARGET_ARCH="armv8-a"\
    SCM_CC_BACKEND="compiler-arm64.scm" \
    SCM_NCC="./compiler-arm.o" -j
rsync -avz -e "ssh -p ${PORT}" --exclude "*.out" --exclude "qemu-env" --exclude ".git" . root@localhost:~/scheme-compiler
ssh -p ${PORT} root@localhost "\
    cd ~/scheme-compiler/ &&\
    source activate.sh &&\
    time gcc -c runtime.c &&\
    time gcc runtime.o compiler-arm.o -o compiler-arm64.out &&\
    time make tests TARGET_ARCH=armv8-a SCM_CC="./compiler-arm64.out" -j &&\
    time make bootstrap-test TARGET_ARCH=armv8-a SCM_CC_BACKEND="compiler-arm64.scm" SCM_CC="./compiler-arm64.out" -j"
rsync -avz -e "ssh -p ${PORT}" root@localhost:~/scheme-compiler/compiler-arm64.out .
exit 0
