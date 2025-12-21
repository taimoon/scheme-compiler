(library
  (compiler-riscv64)
  (export
    compile-prog
    concat-object
    compile-main)
  (import
    (except
      (chezscheme)
      path-extension))
  (begin
    (include "compiler-riscv64.scm")))
