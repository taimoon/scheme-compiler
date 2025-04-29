(library
  (regalloc)
  (export
    assign-home-program
    lower-let-program)
  (import
    (except (chezscheme) partition))
  (include "tmp-alloc.scm"))
