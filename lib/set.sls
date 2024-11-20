(library
  (set)
  (export
    list->set
    make-set
    set-empty?
    set-adjoin
    set-union
    set-diff
    set-intersection
    set-symmetric-diff
    set-equal?
    subset>=?
    subset<=?
    )
  (import
    (rnrs (6))
    (only (chezscheme) include))
  (include "set.scm"))
