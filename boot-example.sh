./compiler.out --make-prim-lib primitives.scm
./compiler.out --combine lib.o \
  intern.scm kernel.scm primitives.scm \
  lib/scheme-libs.scm lib/writer.scm lib/reader.scm
./compiler.out --combine cplib.o lib/match-defmacro.scm lib/set.scm lib/utils.scm
./compiler.out --combine front.o front.scm
./compiler.out --combine compiler.o compiler.scm
./compiler.out -o compiler-1.out lib.o cplib.o front.o compiler.o
echo "====done bootstrap===="
./compiler-1.out -o a.out lib.o test/test-arithmetic.scm
./a.out