SCM_CC = ./compiler.out
LIB_SRCS = intern.scm kernel.scm prim.scm lib/scheme-libs.scm lib/writer.scm lib/reader.scm
LIB_OBJS = $(LIB_SRCS:.scm=.o)
SCM_CC_SRCS = lib/match-defmacro.scm lib/set.scm lib/utils.scm desugar.scm front.scm compiler.scm
SCM_CC_OBJS = $(SCM_CC_SRCS:.scm=.o)

TESTS =  test/test-lit.out \
	test/test-unary.out \
	test/test-binary-cmp.out \
	test/test-arithmetic.out \
	test/test-fx-0.out \
	test/test-let.out \
	test/test-if.out \
	test/test-logic-conn.out \
	test/test-cond.out \
	test/test-pair.out \
	test/test-string.out \
	test/test-vector.out \
	test/test-simple-procedure.out \
	test/test-simple-rec-proc.out \
	test/test-mutual-procedure.out \
	test/test-procedure-ref.out \
	test/test-lambda.out \
	test/test-lambda-1.out \
	test/test-tail-sum.out \
	test/test-tail-add.out \
	test/test-simple-set.out \
	test/test-free-var-set.out \
	test/test-letrec.out \
	test/test-named-let.out \
	test/test-simple-define.out \
	test/test-include-driver.out \
	test/test-bitwise.out \
	test/test-vararg.out \
	test/test-dotted-app.out \
	test/test-simple-gc-0.out \
	test/test-simple-gc-1.out \
	test/test-simple-gc-2.out \
	test/test-simple-gc-3.out \
	test/test-apply.out \
	test/test-apply-apply.out \
	test/test-tail-apply.out \
	test/test-case-lambda-0.out \
	test/test-case-lambda-1.out \
	test/test-case-lambda-2.out \
	test/test-case-lambda-3.out \
  	test/test-operand-prim.out \
	test/test-operand-vector.out \
  	test/test-quasiquote.out \
  	test/test-match.out \
  	test/test-reader.out \

LIB_TST_SRC_1 = test/test-lib-mut.scm test/test-lib-mut-driver.scm
LIB_TST_TGT_1 = test/test-lib-mut-driver.out
LIB_TST_SRC_2 = test/test-lib-lib.scm test/test-lib-driver.scm
LIB_TST_TGT_2 = test/test-lib-driver.out
LIB_TST_SRC_3 = test/test-lib-free-lib.scm test/test-lib-free-driver.scm
LIB_TST_TGT_3 = test/test-lib-free-driver.out

MULTI_FILE_TGT = $(LIB_TST_TGT_1) $(LIB_TST_TGT_2) $(LIB_TST_TGT_3) test/test-ffi-io.out test/test-command-line.out

$(LIB_TST_TGT_1) : lib.o $(LIB_TST_SRC_1)
	$(SCM_CC) -o $@ lib.o $(LIB_TST_SRC_1)
	diff <(./$@) $*.txt

$(LIB_TST_TGT_2) : lib.o $(LIB_TST_SRC_2)
	$(SCM_CC) -o $@ lib.o $(LIB_TST_SRC_2)
	diff <(./$@) $*.txt

$(LIB_TST_TGT_3) : lib.o $(LIB_TST_SRC_3)
	$(SCM_CC) -o $@ lib.o $(LIB_TST_SRC_3)
	diff <(./$@) $*.txt

test/test-ffi-io.out : lib.o test/test-ffi-io.scm
	$(SCM_CC) -o $@ lib.o test/test-ffi-io.scm
	diff <(./$@) $*.scm
	diff $*.scm $*.txt

test/test-command-line.out : lib.o test/test-command-line.scm
	$(SCM_CC) -o $@ lib.o $*.scm
	diff <(./$@ reimu marisa -19 2 3 5) $*.txt

runtime.o:
	gcc -fno-omit-frame-pointer -m32 runtime.c -c -o runtime.o

prim.scm:
	$(SCM_CC) --make-prim-lib prim.scm

$(LIB_OBJS) : %.o : %.scm
	$(SCM_CC) -o $@ $<

$(SCM_CC_OBJS) : %.o : %.scm
	$(SCM_CC) -o $@ $<

lib.o: $(LIB_OBJS)
	$(SCM_CC) -o lib.o $(LIB_OBJS)

$(SCM_NCC): runtime.o lib.o $(SCM_CC_OBJS)
	$(SCM_CC) -o $(SCM_NCC) lib.o $(SCM_CC_OBJS)

new-compiler: $(SCM_NCC)

$(TESTS) : %.out : %.scm lib.o runtime.o
	$(SCM_CC) -o ./$@ lib.o $<
	diff <(./$@) $*.txt

.PHONY: tests-seq
tests-seq:
	scheme --script run-test.scm "$(SCM_CC)" --bootstrap

.PHONY: tests
tests: $(TESTS) $(MULTI_FILE_TGT)
	rm -rf $(TESTS) $(MULTI_FILE_TGT)


.PHONY: bootstrap-test
bootstrap-test:
	make rebuild-clean
	time (make new-compiler SCM_CC="$(SCM_CC)" SCM_NCC="./compiler-new.out")
	time (make tests SCM_CC="./compiler-new.out")
	(echo "##################") >> /dev/stderr
	##########################################################################################
	make clean
	time (make compiler-0.out SCM_CC="./compiler-new.out" SCM_NCC="./compiler-0.out")
	time (make tests SCM_CC="./compiler-0.out")
	(echo "##################") >> /dev/stderr
	##########################################################################################
	make clean
	time (make compiler-1.out SCM_CC="./compiler-0.out" SCM_NCC="./compiler-1.out")
	time (make tests SCM_CC="./compiler-1.out")
	make clean
	(echo "##################") >> /dev/stderr
	##########################################################################################
	rm -f /dev/shm/scm-build*


.PHONY: update
update: bootstrap-test
	mv ./compiler-1.out $(SCM_CC)

.PHONY: clean
clean:
	rm -f $(LIB_OBJS) $(SCM_CC_OBJS) compiler.o runtime.o lib.o prim.scm test/*.out

.PHONY: rebuild-clean
rebuild-clean: clean
	rm -f compiler-1.out compiler-0.out compiler-new.out