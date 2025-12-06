#!/bin/bash
set -xe
source activate.sh
make \
    new-compiler \
    SCM_CC="./compiler-x86_64.out" \
    TARGET_ARCH="armv8-a" \
    SCM_CC_BACKEND="compiler-arm64.scm" \
    SCM_NCC="./compiler-arm64-x86_64.out" -j
CC=aarch64-linux-gnu-gcc OBJCOPY=aarch64-linux-gnu-objcopy make new-compiler \
    fLINK_RUNTIME=0 \
    SCM_CC="./compiler-arm64-x86_64.out" \
    TARGET_ARCH="armv8-a"\
    SCM_CC_BACKEND="compiler-arm64.scm" \
    SCM_NCC="./compiler-arm.o" -j
PORT=22223
HOST_NAME=root@localhost
rsync -avz -e "ssh -p ${PORT}" --exclude "*.out" --exclude "qemu-env" --exclude ".git" . ${HOST_NAME}:~/scheme-compiler
ssh -p ${PORT} ${HOST_NAME} "cd ~/scheme-compiler/ && TARGET_ARCH=arm64 ./upgrade.sh"
rsync -avz -e "ssh -p ${PORT}" ${HOST_NAME}:~/scheme-compiler/compiler-arm64.out .
