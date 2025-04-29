export HEAP_SIZE=$((0xfffff8))
export CHEZSCHEMELIBDIRS=".:./lib/"

# Term		Meaning
# build		System on which compiler is built
# host		System on which compiler itself will run
# target	System on which output of compiler will run
# <CC>-<host>

function test_i686_chez {
    time make tests TARGET_ARCH=i686 SCM_CC="petite --script compiler.so" -j || exit 1
    time make bootstrap-test TARGET_ARCH=i686 SCM_CC_BACKEND="compiler.scm" SCM_CC="petite --script compiler.so" -j || exit 1
}

function test_x64_chez {
    time make tests TARGET_ARCH=x86-64 SCM_CC="petite --script compiler-x86_64.so" -j || exit 1
    time make bootstrap-test TARGET_ARCH=x86-64 SCM_CC_BACKEND="compiler-x86_64.scm" SCM_CC="petite --script compiler-x86_64.so" -j || exit 1
}

function test_i686 {
    time make tests TARGET_ARCH=i686 SCM_CC="./compiler.out" -j || exit 1 ;
    time make bootstrap-test TARGET_ARCH=i686 SCM_CC_BACKEND="compiler.scm" SCM_CC="./compiler.out" -j || exit 1
}

function test_x64_boot_chez {
    time \
    make new-compiler \
        TARGET_ARCH=x86-64 SCM_CC_BACKEND="compiler-x86_64.scm" \
        SCM_CC="scheme --script compiler-x86_64.so" SCM_NCC="./compiler-new.out" -j || exit 1
    time make tests TARGET_ARCH=x86-64 SCM_CC="./compiler-new.out" -j || exit 1
    mv ./compiler-new.out ./compiler-x86_64-new.out
    ### full bootstrap test
    make bootstrap-test TARGET_ARCH=x86-64 SCM_CC_BACKEND="compiler-x86_64.scm" SCM_CC="./compiler-x86_64-new.out" -j || exit 1
}
function test_x64_i686_cross {
    ### testing boot x64 from i686
    echo "cross compiling amd64 from i686"
    time \
    make cross-compile-test \
        BUILD_ARCH=i686 BUILD_CC="./compiler-i686.out" \
        TARGET_ARCH=x86-64 SCM_CC_BACKEND="compiler-x86_64.scm" \
        SCM_NCC="./compiler-x86_64-new.out" -j
    echo "cross compiling i686 from amd64"
    time \
    make cross-compile-test \
        BUILD_ARCH=x86-64 BUILD_CC="./compiler-x86_64-new.out" \
        TARGET_ARCH=i686 SCM_CC_BACKEND="compiler.scm" \
        SCM_NCC="./compiler-i686-new.out" -j
}

make rebuild-clean
time scheme --script make-compiler-chez.scm
test_i686_chez
test_x64_chez
test_i686
test_x64_boot_chez
test_x64_i686_cross
exit 0