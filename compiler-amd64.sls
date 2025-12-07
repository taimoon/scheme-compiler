(library
  (compiler-amd64)
  (export
    compile-prog
    concat-object
    compile-main)
  (import
    (except
      (chezscheme)
      path-extension))
  (begin
    (include "compiler-amd64.scm")))
