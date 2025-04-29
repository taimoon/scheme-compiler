export HEAP_SIZE=$((0xfffff8))
export CHEZSCHEMELIBDIRS=".:./lib/"
# # If you have chez scheme
# make rebuild-clean
# make lib.o runtime.o -j > /dev/null
# scheme --script make-compiler-chez.scm
# time make tests SCM_CC="petite --script compiler.so" -j
# ############################
make rebuild-clean
time make bootstrap-test SCM_CC="./compiler.out" -j > /dev/null