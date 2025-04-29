(library
  (desugar)
  (export
    immediate?
    write-primitive-lib
    linearize-seq
    make-begin
    make-let
    uniquify-program)
  (import
    (chezscheme))
  (include "desugar.scm"))
