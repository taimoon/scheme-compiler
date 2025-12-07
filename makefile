# = lazy expansion
# := strict expansion
# ?= assigned only if undefined
# += append
SHELL := /bin/bash
CC ?= gcc
CFLAGS ?= -g -Wall -fno-omit-frame-pointer
TARGET_ARCH ?= amd64
BOOTSTRAP_TEST ?= 0
ARM_HOST_NAME ?= 
ARM_WORKDIR ?= /tmp/scheme-compiler
CROSS_FLAG ?= 0

ifeq ($(TARGET_ARCH),amd64)
	CFLAGS += -m64
	SCM_RIDER := compiler-rider-amd64.scm
	SCM_BACKEND := compiler-amd64.scm
else ifeq ($(TARGET_ARCH),i686)
	CFLAGS += -m32
	SCM_RIDER := compiler-rider-i686.scm
	SCM_BACKEND := compiler-i686.scm
else ifeq ($(TARGET_ARCH),arm64)
	SCM_RIDER := compiler-rider-arm64.scm
	SCM_BACKEND := compiler-arm64.scm
else
    $(error "Unknown machine architecture $(TARGET_ARCH)")
endif

SCM_CC ?= scheme --script compiler.so
NPROC ?= $(shell nproc)
SCM_NCC ?= ./compiler.out
SCM_RUNTIME ?= runtime.so
SCM_RUNTIME_OBJ ?= $(SCM_RUNTIME:.so=.o)

TESTS := \
	test/test-lit.scm \
	test/test-imm.scm \
	test/test-let.scm \
	test/test-arithmetic.scm \
	test/test-unary-int.scm \
	test/test-unary-pred-imm.scm \
	test/test-unary-char.scm \
	test/test-bitwise.scm \
	test/test-if.scm \
	test/test-abs.scm \
	test/test-str-0.scm \
	test/test-str-1.scm \
	test/test-vec.scm \
	test/test-bytevec.scm \
	test/test-pair.scm \
	test/test-assign-0.scm \
	test/test-proc-0.scm \
	test/test-proc-1.scm \
	test/test-assign-1.scm \
	test/test-proc-rec-0.scm \
	test/test-proc-rec-1.scm \
	test/test-tail-mul.scm \
	test/test-internal-defn.scm \
	test/test-vararg.scm \
	test/test-symbol-prim.scm \
	test/test-apply.scm \
	$(if $(LIB), test/test-apply-apply.scm) \
	$(if $(LIB), test/test-apply-more.scm) \
	test/test-case-lambda-0.scm \
	test/test-case-lambda-1.scm \
	test/test-case-lambda-2.scm \
	test/test-include/rider.scm \
	test/test-ffi-write-char.scm \
	$(if $(LIB), test/test-quasiquote.scm) \
	$(if $(LIB), test/test-match.scm) \
	$(if $(LIB), test/test-reader.scm) \
	$(if $(LIB), test/test-utf8-read.scm) \
	$(if $(LIB), test/test-utf8-write.scm) \
	$(if $(LIB), test/test-read-char.scm) \
	test/gc/test-simple-gc-0.scm \
	test/gc/test-simple-gc-1.scm \
	test/gc/test-simple-gc-2.scm \
	test/gc/test-simple-gc-3.scm \
	test/z-combinator.scm \
	test/z-combinator-mutual.scm \
	test/man-or-boy.scm \
	test/test-cxr.scm \
	$(if $(LIB), test/test-interp.scm) \
	$(if $(LIB), test/eval/test-eval-0.scm) \
	test/values/test-values-0.scm \
	test/values/test-values-1.scm \
	test/values/test-values-2.scm \
	test/values/test-values-unwoke.scm \
	test/values/test-nested-values-0.scm \
	test/values/partition.scm \
	$(if $(LIB), test/values/test-operand-cwv.scm) \
	$(if $(LIB), test/values/test-operand-values.scm) \
	test/control/test-callcc-0.scm \
	test/control/test-callcc-1.scm \
	test/control/test-callcc-2.scm \
	test/control/test-callcc-3.scm \
	test/control/test-callcc-4.scm \
	test/control/test-callcc-5.scm \
	test/control/test-callcc-6-iter.scm \
	test/control/test-callcc-6-recur.scm \
	test/control/test-callcc-7-tail.scm \
	test/control/test-callcc-8-tail.scm \
	test/control/test-coro-0.scm \
	test/control/test-coro-1.scm \
	test/control/test-coro-2.scm \
	test/control/test-callcc-values-0.scm \
	test/control/test-callcc-values-1.scm \
	test/control/test-callcc-values-2.scm \
	test/control/test-callcc-values-unwoke.scm \
	test/control/partition.scm \
	$(if $(LIB), test/control/test-operand-callcc-0.scm) \
	$(if $(LIB), test/control/test-operand-callcc-1.scm) \
	$(if $(LIB), test/control/test-dynwind.scm) \

LIB_OBJS := \
	intern.o \
	prim.o \
	lib/unicode.o \
	lib/dynwind.o \
	lib/scheme-libs.o \
	lib/writer.o \
	lib/reader.o \
	lib/set.o \
	lib/utils.o \
	front.o \
	$(SCM_BACKEND:.scm=.o)

PHONY: make_new_compiler
make_new_compiler: $(SCM_NCC)

PHONY: make_runtime
make_runtime: $(SCM_RUNTIME) $(SCM_RUNTIME)

$(SCM_RUNTIME_OBJ):
	$(CC) $(CFLAGS) -fPIC -o $@ -c runtime.c

$(SCM_RUNTIME): $(SCM_RUNTIME_OBJ)
	$(CC) $(CFLAGS) -shared -o $@ $^

prim.o:
	$(SCM_CC) --prim $@

$(filter-out prim.o,$(LIB_OBJS)): %.o : %.scm
	_DEBUG_LIVE_RATIO_FLAG="OFF" HEAP_WORDS_MAX=10240 $(SCM_CC) -o $@ $<

psyntax.o: lib/hashtables.scm compat.scm
	$(SCM_CC) -o $@ $^

lib.o : $(LIB_OBJS)
	$(SCM_CC) -o $@ $^
	rm -f $(LIB_OBJS) 

$(SCM_NCC): lib.o $(SCM_RIDER)
	$(SCM_CC) -o $(SCM_NCC) $^
	rm -f lib.o

_TST_DUMMY_EXT_0 := .tst_0
_TST_DUMMY_TGT_0 := $(TESTS:.scm=$(_TST_DUMMY_EXT_0))

$(_TST_DUMMY_TGT_0) : %$(_TST_DUMMY_EXT_0) : $(LIB) %.scm %.txt
	diff <($(SCM_CC) $(LIB) $*.scm) $*.txt

test/test-intern.tst: %.tst : $(if $(LIB), $(LIB), intern.scm) %.scm
	diff <($(SCM_CC) $^) $*.txt

test/test-quote-0.tst: %.tst : $(if $(LIB), $(LIB), intern.scm) %.scm
	diff <($(SCM_CC) $^) $*.txt

test/gc/test-simple-gc-4.tst: %.tst : $(if $(LIB), $(LIB), intern.scm) %.scm
	diff <($(SCM_CC) $^) $*.txt

test/test-include/driver.tst: %.tst : $(if $(LIB), $(LIB), intern.scm) test/test-include/odd.scm test/test-include/even.scm %.scm
	diff <($(SCM_CC) $^) $*.txt

test/test-operand-prim.tst: %.tst : $(if $(LIB), $(LIB), intern.scm prim.o) %.scm
	diff <($(SCM_CC) $^) $*.txt

test/test-getenv.tst: %.tst : $(LIB) %.scm
	diff <(GETENV_TEST_VALUE="简体" $(SCM_CC) $^) $*.txt

test/test-command-line.tst: %.tst : $(LIB) %.scm
	$(SCM_CC) -o $*.out $^
	diff <($*.out "博丽灵梦" "雾雨魔理沙") <(echo "(\"$*.out\" \"博丽灵梦\" \"雾雨魔理沙\")")
	rm -f $*.out

_TESTS :=  $(_TST_DUMMY_TGT_0) \
	test/test-intern.tst \
	test/test-quote-0.tst \
	test/gc/test-simple-gc-4.tst \
	test/test-operand-prim.tst \
	test/test-include/driver.tst \
	$(if $(LIB), test/test-getenv.tst) \
	$(if $(LIB), test/test-command-line.tst) \

.PHONY: _test
_test: $(_TESTS)

.PHONY: from_chez
from_chez:
	scheme --script make-compiler-chez.scm $(SCM_RIDER) $(SCM_BACKEND:.scm=.sls)

.PHONY: bootstrap_3
bootstrap_3:
ifeq ($(BOOTSTRAP_TEST),1)
	time (make _test -j$(NPROC) SCM_CC="FOREIGN_IO=TRUE PRIM_CALLCC=TRUE $(SCM_CC)")
	time (make _test -j$(NPROC) SCM_CC="$(SCM_CC)" LIB="lib.o")
endif
	time (make make_new_compiler -j$(NPROC) SCM_CC="$(SCM_CC)" SCM_NCC="./compiler.out")
ifeq ($(BOOTSTRAP_TEST),1)
	time (make _test -j$(NPROC) SCM_CC="FOREIGN_IO=TRUE PRIM_CALLCC=TRUE ./compiler.out")
	time (make _test -j$(NPROC) SCM_CC="./compiler.out" LIB="lib.o")
endif
	time (make make_new_compiler -j$(NPROC) SCM_CC="./compiler.out" SCM_NCC="./compiler-0.out")
ifeq ($(BOOTSTRAP_TEST),1)
	time (make _test -j$(NPROC) SCM_CC="FOREIGN_IO=TRUE PRIM_CALLCC=TRUE ./compiler-0.out")
	time (make _test -j$(NPROC) SCM_CC="./compiler-0.out" LIB="lib.o")
endif
	time (make make_new_compiler -j$(NPROC) SCM_CC="./compiler-0.out" SCM_NCC="$(SCM_NCC)")
ifeq ($(BOOTSTRAP_TEST),1)
	time (make _test -j$(NPROC) SCM_CC="FOREIGN_IO=TRUE PRIM_CALLCC=TRUE $(SCM_NCC)")
	time (make _test -j$(NPROC) SCM_CC="$(SCM_NCC)" LIB="lib.o")
endif

.PHONY: test_one_target
test_one_target:
	make bootstrap_3 BOOTSTRAP_TEST=1

.PHONY: cross_compile
cross_compile:
	make make_new_compiler -j$(NPROC) SCM_RUNTIME="$(SCM_RUNTIME)" SCM_CC="$(SCM_CC)" \
		TARGET_ARCH=$(XTARGET_ARCH) SCM_NCC=$(SCM_XCC)
	make lib.o -j$(NPROC) LIB="lib.o" SCM_RUNTIME="$(SCM_RUNTIME)" SCM_CC=$(SCM_XCC) \
		CC=$(XGNU_PREFIX)gcc OBJCOPY=$(XGNU_PREFIX)objcopy \
		TARGET_ARCH=$(XTARGET_ARCH)
	CC=$(XGNU_PREFIX)gcc OBJCOPY=$(XGNU_PREFIX)objcopy $(SCM_XCC) -c $(SCM_NCC_OBJ) lib.o $(SCM_RIDER)
	rm -f lib.o

.PHONY: cross_compile_arm64_amd64
cross_compile_arm64_amd64:
	make make_runtime SCM_RUNTIME=runtime64.so TARGET_ARCH=amd64
	make cross_compile NPROC=$(NPROC) \
		SCM_RUNTIME=runtime64.so SCM_CC="./compiler-amd64.out" \
		XTARGET_ARCH=arm64 \
		XGNU_PREFIX="aarch64-linux-gnu-" \
		SCM_XCC="./compiler-arm64-amd64.out" \
		SCM_NCC_OBJ="./compiler-arm64.o"

.PHONY: test_i686_chez
test_i686_chez:
	# cross-compile from chez to i686
	make from_chez TARGET_ARCH=i686
	make make_runtime SCM_RUNTIME=runtime32.so TARGET_ARCH=i686
	make test_one_target NPROC=$(NPROC) TARGET_ARCH=i686 SCM_RUNTIME=runtime32.so SCM_CC="scheme --script compiler.so" SCM_NCC="./compiler-i686.out"

.PHONY: test_amd64_chez
test_amd64_chez:
	# cross-compile from chez to amd64
	make from_chez TARGET_ARCH=amd64
	make make_runtime SCM_RUNTIME=runtime64.so TARGET_ARCH=amd64
	make test_one_target NPROC=$(NPROC) TARGET_ARCH=amd64 SCM_RUNTIME="runtime64.so" SCM_CC="scheme --script compiler.so" SCM_NCC="./compiler-amd64.out"

.PHONY: test_arm64
test_arm64:
ifeq ($(CROSS_FLAG),1)
	make make_runtime SCM_RUNTIME=runtime.so
	$(CC) $(CFLAGS) $(SCM_NCC_OBJ) runtime.so -o $(SCM_CC)
	rm -f $(SCM_NCC_OBJ)
	make test_one_target NPROC=$(NPROC) SCM_RUNTIME=runtime.so SCM_CC=$(SCM_CC) SCM_NCC="./compiler-arm64-new.out"
else
	make clean
	make make_runtime SCM_RUNTIME=runtime.so
	make from_chez TARGET_ARCH=arm64
	make test_one_target NPROC=$(NPROC) SCM_RUNTIME=runtime.so SCM_CC="scheme --script compiler.so" SCM_NCC="./compiler-arm64-new.out"
endif

.PHONY: test_arm64_cross
test_arm64_cross:
	rsync -avh \
		--exclude=".git" \
		--exclude="*.so" \
		--exclude="*.o" \
		--exclude="*.out" \
		. ${ARM_HOST_NAME}:${ARM_WORKDIR}
	ssh ${ARM_HOST_NAME} "cd ${ARM_WORKDIR} && make clean"
	make cross_compile_arm64_amd64
	rsync -avh compiler-arm64.o ${ARM_HOST_NAME}:${ARM_WORKDIR}/
	ssh ${ARM_HOST_NAME} \
		"cd ${ARM_WORKDIR} && \
		source env.sh && \
		make test_arm64 TARGET_ARCH=arm64 CROSS_FLAG=1 SCM_NCC_OBJ=compiler-arm64.o SCM_CC=./compiler-arm64.out"
	rsync -avh ${ARM_HOST_NAME}:${ARM_WORKDIR}/compiler-arm64.out .

.PHONY: test
test:
	make clean
	make test_amd64_chez
ifneq ($(strip $(ARM_HOST_NAME)),)
	make test_arm64_cross
endif
	make test_i686_chez
	# cross-compile from i686 to amd64
	make cross_compile NPROC=$(NPROC) \
		SCM_RUNTIME=runtime32.so SCM_CC="./compiler-i686.out" \
		XTARGET_ARCH=amd64 \
		SCM_XCC="./compiler-amd64-i686.out" \
		SCM_NCC_OBJ="./compiler-amd64-new.o"
	
	$(CC) ./compiler-amd64-new.o runtime64.so -o ./compiler-amd64-new.out
	make bootstrap_3 NPROC=$(NPROC) TARGET_ARCH=amd64 SCM_RUNTIME=runtime64.so SCM_CC="./compiler-amd64-new.out" SCM_NCC="./compiler-2.out"

	# cros-compile from amd64 to i686
	make cross_compile NPROC=$(NPROC) \
		SCM_RUNTIME=runtime64.so SCM_CC="./compiler-amd64.out" \
		XTARGET_ARCH=i686 \
		SCM_XCC="./compiler-i686-amd64.out" \
		SCM_NCC_OBJ="./compiler-i686-new.o"
	
	$(CC) -m32 ./compiler-i686-new.o runtime32.so -o ./compiler-i686-new.out
	make bootstrap_3 NPROC=$(NPROC) TARGET_ARCH=i686 SCM_RUNTIME=runtime32.so SCM_CC="./compiler-i686-new.out" SCM_NCC="./compiler-2.out"

.PHONY: make_amd64_compiler_chez
make_amd64_compiler_chez:
	make make_runtime SCM_RUNTIME=runtime.so
	make from_chez TARGET_ARCH=amd64
	make make_runtime SCM_RUNTIME=runtime.so
	make make_new_compiler TARGET_ARCH=amd64 SCM_CC="scheme --script compiler.so" SCM_NCC="./compiler-amd64.out" -j

.PHONY: clean
clean:
	rm -f *.so *.o lib/*.o *.out
