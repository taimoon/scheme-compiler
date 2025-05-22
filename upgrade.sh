export HEAP_SIZE=$((0xfffff8))
export CHEZSCHEMELIBDIRS=".:./lib/"

# Term		Meaning
# build		System on which compiler is built
# host		System on which compiler itself will run
# target	System on which output of compiler will run
# <CC>-<host>

function test_i686_chez {
    make rebuild-clean
    time make tests TARGET_ARCH=i686 SCM_CC="petite --script compiler.so" -j && \
    time make bootstrap-test TARGET_ARCH=i686 SCM_CC_BACKEND="compiler.scm" SCM_CC="petite --script compiler.so" -j && \
    rm compiler.so
}

function test_x64_chez {
    make rebuild-clean
    time make tests TARGET_ARCH=x86-64 SCM_CC="petite --script compiler-x86_64.so" -j && \
    time make bootstrap-test TARGET_ARCH=x86-64 SCM_CC_BACKEND="compiler-x86_64.scm" SCM_CC="petite --script compiler-x86_64.so" -j && \
    rm compiler-x86_64.so
}

function test_i686 {
    make rebuild-clean
    time make bootstrap-test TARGET_ARCH=i686 SCM_CC_BACKEND="compiler.scm" SCM_CC="./compiler-i686.out" -j &&\
    mv compiler-2.out ./compiler-i686.out
}

function test_x86 {
    make rebuild-clean
    time make bootstrap-test TARGET_ARCH=x86-64 SCM_CC_BACKEND="compiler-x86_64.scm" SCM_CC="./compiler-x86_64.out" -j &&\
    mv compiler-2.out ./compiler-x86_64.out
}

function test_x64_i686_cross {
    make rebuild-clean
    ### testing boot x64 from i686
    echo "cross compiling amd64 from i686" && \
    time \
    make cross-compile-test \
        BUILD_ARCH=i686 BUILD_CC="./compiler-i686.out" \
        TARGET_ARCH=x86-64 SCM_CC_BACKEND="compiler-x86_64.scm" \
        SCM_NCC="./compiler-x86_64-new.out" -j && \
    echo "cross compiling i686 from amd64" && \
    time \
    make cross-compile-test \
        BUILD_ARCH=x86-64 BUILD_CC="./compiler-x86_64-new.out" \
        TARGET_ARCH=i686 SCM_CC_BACKEND="compiler.scm" \
        SCM_NCC="./compiler-i686-new.out" -j
    rm ./compiler-i686-new.out ./compiler-x86_64-new.out
    
    make rebuild-clean
    ### testing boot i686 from x64
    echo "cross compiling i686 from amd64" && \
    time \
    make cross-compile-test \
        BUILD_ARCH=x86-64 BUILD_CC="./compiler-x86_64.out" \
        TARGET_ARCH=i686 SCM_CC_BACKEND="compiler.scm" \
        SCM_NCC="./compiler-i686-new.out" -j && \
    echo "cross compiling amd64 from i686" && \
    time \
    make cross-compile-test \
        BUILD_ARCH=i686 BUILD_CC="./compiler-i686-new.out" \
        TARGET_ARCH=x86-64 SCM_CC_BACKEND="compiler-x86_64.scm" \
        SCM_NCC="./compiler-x86_64-new.out" -j
    rm ./compiler-i686-new.out ./compiler-x86_64-new.out
    
}

time scheme --script make-compiler-chez.scm &&\
test_i686_chez &&\
test_x64_chez &&\
test_i686 &&\
test_x86 &&\
test_x64_i686_cross &&\
exit 0