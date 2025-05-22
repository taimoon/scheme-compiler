set -x
export HEAP_SIZE=$((0xfffff8))
export CHEZSCHEMELIBDIRS=".:./lib/"
make rebuild-clean
make new-compiler \
    SCM_CC="./compiler-x86_64.out" \
    TARGET_ARCH=rv64 SCM_CC_BACKEND=compiler-riscv64.scm \
    SCM_NCC="compiler-riscv.x86_64.out" -j
export CC=riscv64-linux-gnu-gcc
export OBJCOPY=riscv64-linux-gnu-objcopy
make new-compiler \
    fLINK_RUNTIME=0 \
    SCM_CC="./compiler-riscv.x86_64.out" \
    TARGET_ARCH=rv64 SCM_CC_BACKEND=compiler-riscv64.scm \
    SCM_NCC="compiler-riscv.o" -j
rsync -avz -e "ssh -p 22224" --exclude "*.out" --exclude "qemu-env" --exclude ".git" $PWD root@localhost:~
ssh -p 22224 root@localhost "\
    cd ~/scheme-compiler-v2/ &&\
    source activate.sh &&\
    time gcc -c runtime.c &&\
    time gcc runtime.o compiler-riscv.o -o compiler-riscv.out &&\
    time make tests TARGET_ARCH=rv64 SCM_CC="./compiler-riscv.out" -j &&\
    time make bootstrap-test TARGET_ARCH=rv64 SCM_CC_BACKEND="compiler-riscv64.scm" SCM_CC="./compiler-riscv.out" -j"
rsync -avz -e "ssh -p 22224" root@localhost:~/scheme-compiler-v2/compiler-riscv.out .
exit 0