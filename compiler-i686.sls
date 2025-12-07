(library
  (compiler-i686)
  (export
    compile-prog
    concat-object
    compile-main)
  (import
    (except
      (chezscheme)
      path-extension))
  (begin
    (include "compiler-i686.scm")))
